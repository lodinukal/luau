pub const config = struct {
    /// use longjmp for error handling
    pub const use_longjmp: bool = false;
    /// maximum size for the description of the source
    pub const id_size: isize = 256;
    /// guaranteed number of Lua stack slots available to a C function
    pub const min_stack: isize = 20;
    /// limits the number of Lua stack slots that a C function can use
    pub const max_cstack: isize = 8000;
    /// limits the number of nested calls
    pub const max_calls: isize = 20000;
    /// maximum depth for nested c calls
    pub const max_ccalls: isize = 200;
    /// buffer size used for on stack string operations
    pub const buffer_size: isize = 512;
    /// number of valid Lua userdata tags
    pub const utag_limit: isize = 128;
    /// number of valid Lua lightuserdata tags
    pub const lutag_limit: isize = 128;
    /// upper bound for number of size classes used by page allocator
    pub const size_classes: isize = 40;
    /// available number of separate memory categories
    pub const memory_categories: isize = 256;
    /// minimum size for the string table (must be power of 2)
    pub const min_strtab_size: isize = 32;
    /// maximum number of captures supported by pattern matching
    pub const max_captures: isize = 32;
    /// vector size for the VM
    pub const vector_size: usize = raw.LUA_VECTOR_SIZE;

    pub const extra_size = vector_size - 2;
};

pub const Index = enum(i32) {
    multiret = -1,
    none = 0,

    registry = -config.max_cstack - 2000,
    environ = -config.max_cstack - 2001,
    globals = -config.max_cstack - 2002,

    _,

    pub fn at(i: i32) Index {
        std.debug.assert(i != 0);
        return @enumFromInt(i);
    }

    pub inline fn upvalue(i: i32) i32 {
        return @enumFromInt(@intFromEnum(Index.globals) - i);
    }

    pub inline fn isPseudo(self: Index) bool {
        return @intFromEnum(self) <= @intFromEnum(Index.registry);
    }

    pub inline fn shift(self: Index, n: i32) Index {
        return @enumFromInt(@intFromEnum(self) + n);
    }
};

pub const Status = enum(u32) {
    ok = 0,
    yield,
    err_run,
    deprecated_err_syntax,
    err_mem,
    err_err,
    @"break",
};

pub const CoroutineStatus = enum(u32) {
    running = 0,
    suspended,
    normal,
    finished,
    finished_error,
};

pub const CFunction = *const fn (l: *State) callconv(.c) i32;
pub const Continuation = *const fn (l: *State, status: Status) callconv(.c) i32;

pub const Alloc = *const fn (
    userdata: ?*anyopaque,
    pointer: ?*anyopaque,
    old_size: usize,
    new_size: usize,
) callconv(.c) ?*align(alignment) anyopaque;

const alignment = @alignOf(std.c.max_align_t);
/// Allows Luau to allocate memory using a Zig allocator passed in via data.
pub fn alloc(data: ?*anyopaque, ptr: ?*anyopaque, osize: usize, nsize: usize) callconv(.c) ?*align(alignment) anyopaque {
    // just like malloc() returns a pointer "which is suitably aligned for any built-in type",
    // the memory allocated by this function should also be aligned for any type that Lua may
    // desire to allocate. use the largest alignment for the target
    const allocator_ptr: *std.mem.Allocator = @ptrCast(@alignCast(data.?));

    if (@as(?[*]align(alignment) u8, @ptrCast(@alignCast(ptr)))) |prev_ptr| {
        const prev_slice = prev_ptr[0..osize];

        // when nsize is zero the allocator must behave like free and return null
        if (nsize == 0) {
            allocator_ptr.free(prev_slice);
            return null;
        }

        // when nsize is not zero the allocator must behave like realloc
        const new_ptr = allocator_ptr.realloc(prev_slice, nsize) catch return null;
        return new_ptr.ptr;
    } else if (nsize == 0) {
        return null;
    } else {
        // ptr is null, allocate a new block of memory
        const new_ptr = allocator_ptr.alignedAlloc(u8, alignment, nsize) catch return null;
        return new_ptr.ptr;
    }
}

pub const Type = enum(i32) {
    none = -1,
    nil = 0,
    boolean = 1,
    lightuserdata,
    number,
    vector,

    string, // all types above this must be value types, all types below this must be GC types - see iscollectable

    table,
    function,
    userdata,
    thread,
    buffer,

    // values below this line are used in GCObject tags but may never show up in TValue type tags
    proto,
    upval,
    deadkey,
};

pub const Number = f64;
pub const Integer = i32;
pub const Unsigned = u32;
pub const Vector: type = @Vector(config.vector_size, f32);

pub const Buffer = union(enum) {
    mutable: []u8,
    immutable: []const u8,

    pub inline fn literal(buf: []const u8) Buffer {
        return .{ .immutable = buf };
    }

    pub fn constSlice(self: Buffer) []const u8 {
        return switch (self) {
            .mutable => self.mutable,
            .immutable => self.immutable,
        };
    }
};

pub const Atom = enum(i32) { _ };

pub const State = opaque {
    fn tlua(l: *State) ?*raw.lua_State {
        return @ptrCast(l);
    }

    fn flua(l: ?*raw.lua_State) !*State {
        return @ptrCast(l orelse return error.NullLuaState);
    }

    pub inline fn init(a: *const std.mem.Allocator) !*State {
        raw.registerAssertionHandler();
        return try flua(raw.lua_newstate(alloc, @constCast(a)));
    }

    pub inline fn deinit(l: *State) void {
        raw.lua_close(tlua(l));
    }

    pub fn allocator(l: *State) std.mem.Allocator {
        var data: ?*std.mem.Allocator = undefined;
        _ = raw.lua_getallocf(tlua(l), @ptrCast(&data));

        if (data) |allocator_ptr| {
            // Although the Allocator is passed to Lua as a pointer, return a
            // copy to make use more convenient.
            return allocator_ptr.*;
        }

        @panic("Lua.allocator() invalid on Lua states created without a Zig allocator");
    }

    /// Creates a new thread, pushes it on the stack, and returns a pointer to a lua_State that
    /// represents this new thread. The new state returned by this function shares with the original
    /// state all global objects (such as tables), but has an independent execution stack.
    ///
    /// There is no explicit function to close or to destroy a thread. Threads are subject to garbage
    /// collection, like any Lua object.
    pub inline fn initThread(l: *State) !*State {
        return try flua(raw.lua_newthread(tlua(l)));
    }

    pub inline fn mainThread(l: *State) !*State {
        return try flua(raw.lua_mainthread(tlua(l)));
    }

    pub inline fn resetThread(l: *State) void {
        raw.lua_resetthread(tlua(l));
    }

    pub inline fn isThreadReset(l: *State) bool {
        return raw.lua_isthreadreset(tlua(l));
    }

    // stack manipulation

    /// Converts the acceptable index idx into an equivalent absolute index (that is,
    /// one that does not depend on the stack top).
    pub inline fn absIndex(l: *State, idx: Index) Index {
        return @enumFromInt(raw.lua_absindex(tlua(l), @intFromEnum(idx)));
    }

    /// Returns the index of the top element in the stack. Because indices start at 1,
    /// this result is equal to the number of elements in the stack; in particular, 0 means an empty stack.
    pub inline fn getTop(l: *State) Index {
        return @enumFromInt(raw.lua_gettop(tlua(l)));
    }

    /// Accepts any acceptable index, or 0, and sets the stack top to this index. If the new top is larger
    /// than the old one, then the new elements are filled with nil. If index is 0, then all stack elements
    /// are removed.
    pub inline fn setTop(l: *State, idx: Index) void {
        raw.lua_settop(tlua(l), @intFromEnum(idx));
    }

    /// Pops count value from the stack
    pub inline fn pop(l: *State, count: i32) void {
        raw.lua_settop(tlua(l), -count - 1);
    }

    /// Removes the element at the given valid index, shifting down the elements above this index to fill
    /// the gap. Cannot be called with a pseudo-index, because a pseudo-index is not an actual stack position.
    pub inline fn remove(l: *State, idx: Index) void {
        raw.lua_remove(tlua(l), @intFromEnum(idx));
    }

    /// Moves the top element into the given valid index, shifting up the elements above this index to open space.
    /// Cannot be called with a pseudo-index, because a pseudo-index is not an actual stack position.
    pub inline fn insert(l: *State, idx: Index) void {
        raw.lua_insert(tlua(l), @intFromEnum(idx));
    }

    /// Moves the top element into the given position (and pops it), without shifting any element
    /// (therefore replacing the value at the given position).
    pub inline fn replace(l: *State, idx: Index) void {
        raw.lua_replace(tlua(l), @intFromEnum(idx));
    }

    /// Ensures that there are at least extra free stack slots in the stack. It returns false if
    /// it cannot grow the stack to that size. This function never shrinks the stack; if the stack
    /// is already larger than the new size, it is left unchanged.
    pub inline fn checkStack(l: *State, size: i32) bool {
        return raw.lua_checkstack(tlua(l), size) != 0;
    }

    /// like checkStack but allows for unlimited stack frames
    pub inline fn rawCheckStack(l: *State, size: i32) void {
        raw.lua_checkstack(tlua(l), size);
    }

    /// Exchange values between different threads of the same global state.
    /// This function pops n values from the stack from, and pushes them onto the stack to.
    pub inline fn xmove(from: *State, to: *State, n: i32) void {
        raw.lua_xmove(tlua(from), tlua(to), n);
    }

    /// see xmove, but doesn't pop the values from the stack
    pub inline fn xpush(from: *State, to: *State, n: Index) void {
        raw.lua_xpush(tlua(from), tlua(to), @intFromEnum(n));
    }

    // access functions

    pub inline fn isNumber(l: *State, idx: Index) bool {
        return raw.lua_isnumber(tlua(l), @intFromEnum(idx)) != 0;
    }

    pub inline fn isString(l: *State, idx: Index) bool {
        return raw.lua_isstring(tlua(l), @intFromEnum(idx)) != 0;
    }

    pub inline fn isCFunction(l: *State, idx: Index) bool {
        return raw.lua_iscfunction(tlua(l), @intFromEnum(idx)) != 0;
    }

    pub inline fn isLFunction(l: *State, idx: Index) bool {
        return raw.lua_isLfunction(tlua(l), @intFromEnum(idx)) != 0;
    }

    pub inline fn isUserdata(l: *State, idx: Index) bool {
        return raw.lua_isuserdata(tlua(l), @intFromEnum(idx)) != 0;
    }

    pub inline fn @"type"(l: *State, idx: Index) Type {
        return @enumFromInt(raw.lua_type(tlua(l), @intFromEnum(idx)));
    }

    pub inline fn isNil(l: *State, idx: Index) bool {
        return l.type(idx) == .nil;
    }

    pub inline fn typeName(l: *State, tp: Type) [*:0]const u8 {
        return raw.lua_typename(tlua(l), @intFromEnum(tp));
    }

    pub inline fn equal(l: *State, a: Index, b: Index) bool {
        return raw.lua_equal(tlua(l), @intFromEnum(a), @intFromEnum(b)) != 0;
    }

    pub inline fn rawequal(l: *State, a: Index, b: Index) bool {
        return raw.lua_rawequal(tlua(l), @intFromEnum(a), @intFromEnum(b)) != 0;
    }

    pub inline fn lessThan(l: *State, a: Index, b: Index) bool {
        return raw.lua_lessthan(tlua(l), @intFromEnum(a), @intFromEnum(b)) != 0;
    }

    pub inline fn toNumberx(l: *State, idx: Index) ?Number {
        var isnum: bool align(4) = false;
        const result = raw.lua_tonumberx(tlua(l), @intFromEnum(idx), @ptrCast(@alignCast(&isnum)));
        return if (isnum) result else null;
    }

    pub inline fn toIntegerx(l: *State, idx: Index) ?Integer {
        var isnum: bool align(4) = false;
        const result = raw.lua_tointegerx(tlua(l), @intFromEnum(idx), @ptrCast(@alignCast(&isnum)));
        return if (isnum) result else null;
    }

    pub inline fn toUnsignedx(l: *State, idx: Index) ?Unsigned {
        var isnum: bool align(4) = false;
        const result = raw.lua_tounsignedx(tlua(l), @intFromEnum(idx), @ptrCast(@alignCast(&isnum)));
        return if (isnum) result else null;
    }

    pub inline fn toVector(l: *State, idx: Index) *[config.vector_size]Number {
        return @ptrCast(raw.lua_tovector(tlua(l), @intFromEnum(idx)));
    }

    pub inline fn toBoolean(l: *State, idx: Index) bool {
        return raw.lua_toboolean(tlua(l), @intFromEnum(idx)) != 0;
    }

    pub inline fn toLengthString(l: *State, idx: Index) [:0]const u8 {
        var len: usize = 0;
        return if (raw.lua_tolstring(tlua(l), @intFromEnum(idx), &len)) |ptr| ptr[0..len :0] else "";
    }

    pub inline fn toStringAtom(l: *State, idx: Index, atom: ?*Atom) [*:0]const u8 {
        return @ptrCast(raw.lua_tostringatom(tlua(l), @intFromEnum(idx), @ptrCast(atom)));
    }

    pub inline fn toLengthStringAtom(l: *State, idx: Index, atom: ?*Atom) [:0]const u8 {
        var len: usize = 0;
        return raw.lua_tolstringatom(tlua(l), @intFromEnum(idx), &len, @ptrCast(atom))[0..len :0];
    }

    pub inline fn nameCallAtom(l: *State, atom: ?*Atom) [*:0]const u8 {
        return @ptrCast(raw.lua_namecallatom(tlua(l), @ptrCast(atom)));
    }

    pub inline fn objLen(l: *State, idx: Index) i32 {
        return raw.lua_objlen(tlua(l), @intFromEnum(idx));
    }

    pub inline fn toCFunction(l: *State, idx: Index) CFunction {
        return @ptrCast(raw.lua_tocfunction(tlua(l), @intFromEnum(idx)));
    }

    pub inline fn toLightUserdata(l: *State, idx: Index) ?*anyopaque {
        return raw.lua_touserdata(tlua(l), @intFromEnum(idx));
    }

    pub inline fn toLightUserdataTagged(l: *State, idx: Index, tag: i32) ?*anyopaque {
        return raw.lua_touserdatatagged(tlua(l), @intFromEnum(idx), tag);
    }

    pub inline fn toUserdata(l: *State, idx: Index) ?*anyopaque {
        return raw.lua_touserdata(tlua(l), @intFromEnum(idx));
    }

    pub inline fn toUserdataTagged(l: *State, idx: Index, tag: i32) *anyopaque {
        return raw.lua_touserdatatagged(tlua(l), @intFromEnum(idx), tag);
    }

    pub inline fn userdataTag(l: *State, idx: Index) i32 {
        return raw.lua_userdatatag(tlua(l), @intFromEnum(idx));
    }

    pub inline fn lightUserdataTag(l: *State, idx: Index) i32 {
        return raw.lua_lightuserdatatag(tlua(l), @intFromEnum(idx));
    }

    pub inline fn toThread(l: *State, idx: Index) !*State {
        return try flua(raw.lua_tothread(tlua(l), @intFromEnum(idx)));
    }

    pub inline fn toBuffer(l: *State, idx: Index) Buffer {
        var len: usize = 0;
        const ptr: [*]u8 = @ptrCast(raw.lua_tobuffer(tlua(l), @intFromEnum(idx), &len)[0..len] orelse return &.{});
        return .{ .mutable = ptr[0..len] };
    }

    pub inline fn toPointer(l: *State, idx: Index) ?*const anyopaque {
        // return lua_topointer(l, idx);
        return raw.lua_topointer(tlua(l), @intFromEnum(idx));
    }

    // push functions

    pub inline fn pushNil(l: *State) void {
        raw.lua_pushnil(tlua(l));
    }

    pub inline fn pushNumber(l: *State, n: Number) void {
        raw.lua_pushnumber(tlua(l), n);
    }

    pub inline fn pushInteger(l: *State, n: Integer) void {
        raw.lua_pushinteger(tlua(l), n);
    }

    pub inline fn pushUnsigned(l: *State, n: Unsigned) void {
        raw.lua_pushunsigned(tlua(l), n);
    }

    pub inline fn pushVector(l: *State, vector: Vector) void {
        if (config.vector_size == 4) {
            raw.lua_pushvector(tlua(l), vector[0], vector[1], vector[2], vector[3]);
        } else {
            raw.lua_pushvector(tlua(l), vector[0], vector[1], vector[2]);
        }
    }

    pub inline fn pushLengthString(l: *State, s: []const u8) void {
        raw.lua_pushlstring(tlua(l), s.ptr, @intCast(s.len));
    }

    pub inline fn pushString(l: *State, s: [*:0]const u8) void {
        raw.lua_pushstring(tlua(l), s);
    }

    inline fn isArgComptimeKnown(value: anytype) bool {
        return @typeInfo(@TypeOf(.{value})).@"struct".fields[0].is_comptime;
    }

    /// Push a zig comptime formatted string onto the stack
    pub fn pushFmtString(l: *State, comptime fmt: []const u8, args: anytype) !void {
        if (isArgComptimeKnown(args))
            l.pushLengthString(std.fmt.comptimePrint(fmt, args))
        else {
            const lua_allocator = l.allocator();
            const str = try std.fmt.allocPrint(lua_allocator, fmt, args);
            defer lua_allocator.free(str);
            l.pushLengthString(str);
        }
    }

    pub inline fn pushClosurek(
        l: *State,
        f: CFunction,
        debug_name: [*:0]const u8,
        upvalues: i32,
        cont: ?Continuation,
    ) void {
        raw.lua_pushcclosurek(tlua(l), @ptrCast(f), debug_name, upvalues, @ptrCast(cont));
    }

    pub inline fn pushClosure(l: *State, f: CFunction, debug_name: [*:0]const u8, upvalues: i32) void {
        l.pushClosurek(f, debug_name, upvalues, null);
    }

    pub inline fn pushCFunction(l: *State, f: CFunction, debug_name: [*:0]const u8) void {
        l.pushClosure(f, debug_name, 0);
    }

    pub inline fn pushFunction(l: *State, comptime f: anytype, debug_name: [*:0]const u8) void {
        l.pushCFunction(toCFn(f), debug_name);
    }

    pub inline fn pushBoolean(l: *State, b: bool) void {
        raw.lua_pushboolean(tlua(l), if (b) 1 else 0);
    }

    /// returns true if the main thread
    pub inline fn pushThread(l: *State) bool {
        return raw.lua_pushthread(tlua(l)) != 0;
    }

    pub inline fn pushLightUserdataTagged(l: *State, p: *anyopaque, tag: i32) void {
        raw.lua_pushlightuserdatatagged(tlua(l), p, tag);
    }

    pub inline fn pushLightUserdata(l: *State, p: *anyopaque) void {
        return l.pushLightUserdataTagged(p, 0);
    }

    pub inline fn newUserdataTagged(l: *State, sz: usize, tag: i32) ?*anyopaque {
        return raw.lua_newuserdatatagged(tlua(l), sz, tag);
    }

    pub inline fn newUserdataTaggedWithMetatable(l: *State, sz: usize, tag: i32) ?*anyopaque {
        return raw.lua_newuserdatataggedwithmetatable(tlua(l), sz, tag);
    }

    pub inline fn newUserdataDtor(l: *State, sz: usize, dtor: ?*fn (ptr: ?*anyopaque) callconv(.c) void) ?*anyopaque {
        // return lua_newuserdatadtor(l, sz, dtor);
        return raw.lua_newuserdatadtor(tlua(l), sz, dtor);
    }

    pub inline fn newBuffer(l: *State, sz: usize) Buffer {
        const ptr: [*]u8 = @ptrCast(raw.lua_newbuffer(tlua(l), sz) orelse return .literal(""));
        return .{ .mutable = ptr[0..sz] };
    }

    // get functions
    pub inline fn getTable(l: *State, idx: Index) Type {
        return @enumFromInt(raw.lua_gettable(tlua(l), @intFromEnum(idx)));
    }

    pub inline fn getField(l: *State, idx: Index, k: [:0]const u8) Type {
        // return lua_getfield(l, idx, k.ptr);
        return @enumFromInt(raw.lua_getfield(tlua(l), @intFromEnum(idx), k));
    }

    pub inline fn rawGetField(l: *State, idx: Index, k: [:0]const u8) Type {
        // return lua_rawgetfield(l, idx, k);
        return @enumFromInt(raw.lua_rawgetfield(tlua(l), @intFromEnum(idx), k));
    }

    pub inline fn rawGet(l: *State, idx: Index) Type {
        return @enumFromInt(raw.lua_rawget(tlua(l), @intFromEnum(idx)));
    }

    pub inline fn rawGeti(l: *State, idx: Index, n: i32) Type {
        return @enumFromInt(raw.lua_rawgeti(tlua(l), @intFromEnum(idx), n));
    }

    pub inline fn createTable(l: *State, narr: i32, nrec: i32) void {
        raw.lua_createtable(tlua(l), narr, nrec);
    }

    pub inline fn setReadonly(l: *State, idx: Index, enabled: bool) void {
        raw.lua_setreadonly(tlua(l), @intFromEnum(idx), if (enabled) 1 else 0);
    }

    pub inline fn getReadonly(l: *State, idx: Index) bool {
        return raw.lua_getreadonly(tlua(l), @intFromEnum(idx)) != 0;
    }

    pub inline fn setSafeEnv(l: *State, idx: Index, enabled: bool) void {
        // return lua_setsafeenv(l, idx, enabled);
        raw.lua_setsafeenv(tlua(l), @intFromEnum(idx), if (enabled) 1 else 0);
    }

    pub inline fn getMetatable(l: *State, objindex: Index) Index {
        return @enumFromInt(raw.lua_getmetatable(tlua(l), @intFromEnum(objindex)));
    }

    pub inline fn getFenv(l: *State, idx: Index) Index {
        return @enumFromInt(raw.lua_getfenv(tlua(l), @intFromEnum(idx)));
    }

    // set functions

    pub inline fn setTable(l: *State, idx: Index) void {
        return raw.lua_settable(tlua(l), @intFromEnum(idx));
    }

    pub inline fn setField(l: *State, idx: Index, k: [:0]const u8) void {
        return raw.lua_setfield(tlua(l), @intFromEnum(idx), k);
    }

    pub inline fn rawSetField(l: *State, idx: Index, k: [:0]const u8) void {
        return raw.lua_rawsetfield(tlua(l), @intFromEnum(idx), k);
    }

    pub inline fn rawSet(l: *State, idx: Index) void {
        return raw.lua_rawset(tlua(l), @intFromEnum(idx));
    }

    pub inline fn rawSeti(l: *State, idx: Index, n: i32) void {
        return raw.lua_rawseti(tlua(l), @intFromEnum(idx), n);
    }

    pub inline fn setMetatable(l: *State, objindex: Index) void {
        _ = raw.lua_setmetatable(tlua(l), @intFromEnum(objindex));
    }

    pub inline fn setFenv(l: *State, idx: Index) bool {
        return raw.lua_setfenv(tlua(l), @intFromEnum(idx)) != 0;
    }

    // load and call functions

    pub inline fn loadEnv(l: *State, chunkname: [:0]const u8, data: []const u8, env: i32) bool {
        return raw.luau_load(tlua(l), chunkname, data.ptr, @intCast(data.len), env) == 0;
    }

    pub inline fn load(l: *State, chunkname: [:0]const u8, data: []const u8) bool {
        return l.loadEnv(chunkname, data, 0);
    }

    pub inline fn call(l: *State, nargs: i32, nresults: i32) void {
        raw.lua_call(tlua(l), nargs, nresults);
    }

    pub inline fn pcall(l: *State, nargs: i32, nresults: i32, errfunc: Index) Status {
        return @enumFromInt(raw.lua_pcall(tlua(l), nargs, nresults, @intFromEnum(errfunc)));
    }

    // coroutine functions

    pub inline fn yield(l: *State, nresults: i32) Index {
        return @enumFromInt(raw.lua_yield(tlua(l), nresults));
    }

    pub inline fn @"break"(l: *State) CoroutineStatus {
        return @enumFromInt(raw.lua_break(tlua(l)));
    }

    pub inline fn @"resume"(l: *State, from: *State, narg: i32) CoroutineStatus {
        return @enumFromInt(raw.lua_resume(tlua(l), tlua(from), narg));
    }

    pub inline fn resumeError(l: *State, from: *State) i32 {
        return raw.lua_resumeerror(tlua(l), tlua(from));
    }

    pub inline fn status(l: *State) Status {
        return @enumFromInt(raw.lua_status(tlua(l)));
    }

    pub inline fn isYieldable(l: *State) bool {
        return raw.lua_isyieldable(tlua(l)) == 1;
    }

    pub inline fn getThreadData(l: *State) ?*anyopaque {
        return raw.lua_getthreaddata(tlua(l));
    }

    pub inline fn setThreadData(l: *State, data: ?*anyopaque) void {
        return raw.lua_setthreaddata(tlua(l), data);
    }

    pub inline fn coStatus(l: *State, co: *State) CoroutineStatus {
        return raw.lua_costatus(tlua(l), tlua(co));
    }

    // garbage-collection functions
    pub inline fn gc(l: *State, what: GcOp) i32 {
        switch (what) {
            inline .set_goal,
            .set_step_mul,
            .set_step_size,
            => |value, tag| return raw.lua_gc(tlua(l), @intFromEnum(tag), value),
            inline else => |_, tag| return raw.lua_gc(tlua(l), @intFromEnum(tag), 0),
        }
    }

    pub inline fn setMemCategory(l: *State, category: i32) void {
        return raw.lua_setmemcat(tlua(l), category);
    }

    pub inline fn totalBytes(l: *State, category: i32) usize {
        return raw.lua_totalbytes(tlua(l), category);
    }

    pub inline fn err(l: *State) noreturn {
        raw.lua_error(tlua(l));
    }

    pub inline fn errStr(l: *State, comptime fmt: []const u8, args: anytype) noreturn {
        l.pushFmtString(fmt, args) catch l.pushString(fmt);
        l.err();
    }

    pub inline fn debugTrace(l: *State) [:0]const u8 {
        return std.mem.span(raw.lua_debugtrace(tlua(l)));
    }

    // utils (aux)

    pub fn getMetatableRegistry(l: *State, table_name: [:0]const u8) Type {
        return l.getField(.registry, table_name);
    }

    pub fn getGlobal(l: *State, name: [:0]const u8) Type {
        return l.getField(.globals, name);
    }

    pub fn setGlobal(l: *State, name: [:0]const u8) void {
        l.setField(.globals, name);
    }

    pub const Reg = extern struct {
        name: [*:0]const u8 = "",
        func: ?CFunction = null,
    };

    pub inline fn newMetatable(l: *State, key: [:0]const u8) !void {
        if (raw.luaL_newmetatable(l, key.ptr) == 0) {
            return error.FailedToCreateMetatable;
        }
    }

    const LUA_COLIBNAME = "coroutine";
    const LUA_TABLIBNAME = "table";
    const LUA_OSLIBNAME = "os";
    const LUA_STRLIBNAME = "string";
    const LUA_BITLIBNAME = "bit32";
    const LUA_BUFFERLIBNAME = "buffer";
    const LUA_UTF8LIBNAME = "utf8";
    const LUA_MATHLIBNAME = "math";
    const LUA_DBLIBNAME = "debug";
    const LUA_VECLIBNAME = "vector";

    pub fn open(l: *State, libs: OpenLibraries) void {
        if (libs.base) l.require("", @ptrCast(&raw.luaopen_base));
        if (libs.coroutine) l.require(LUA_COLIBNAME, @ptrCast(&raw.luaopen_coroutine));
        if (libs.table) l.require(LUA_TABLIBNAME, @ptrCast(&raw.luaopen_table));
        if (libs.os) l.require(LUA_OSLIBNAME, @ptrCast(&raw.luaopen_os));
        if (libs.string) l.require(LUA_STRLIBNAME, @ptrCast(&raw.luaopen_string));
        if (libs.bit32) l.require(LUA_BITLIBNAME, @ptrCast(&raw.luaopen_bit32));
        if (libs.buffer) l.require(LUA_BUFFERLIBNAME, @ptrCast(&raw.luaopen_buffer));
        if (libs.utf8) l.require(LUA_UTF8LIBNAME, @ptrCast(&raw.luaopen_utf8));
        if (libs.math) l.require(LUA_MATHLIBNAME, @ptrCast(&raw.luaopen_math));
        if (libs.debug) l.require(LUA_DBLIBNAME, @ptrCast(&raw.luaopen_debug));
        if (libs.vector) l.require(LUA_VECLIBNAME, @ptrCast(&raw.luaopen_vector));
    }

    pub fn sandbox(l: *State) void {
        raw.luaL_sandbox(tlua(l));
    }

    pub fn sandboxThread(l: *State) void {
        raw.luaL_sandboxthread(tlua(l));
    }

    pub inline fn require(l: *State, name: [:0]const u8, func: CFunction) void {
        l.pushCFunction(func, name);
        l.pushString(name);
        l.call(1, 0);
    }

    // custom helpers
    pub fn pushVal(l: *State, val: anytype) void {
        const T = @TypeOf(val);
        const ti = @typeInfo(T);
        if (T == []const u8 or T == []u8) {
            l.pushLengthString(val);
        } else if (T == [*:0]const u8 or T == [*:0]u8) {
            l.pushString(val);
        } else if (T == Vector) {
            l.pushVector(val);
        } else if (T == Buffer) {
            const slice = val.constSlice();
            const new_buffer = l.newBuffer(slice.len);
            @memcpy(new_buffer.mutable, slice);
        } else switch (ti) {
            .bool => l.pushBoolean(val),
            .int => |int| {
                if (int.signedness == .signed) {
                    l.pushInteger(@intCast(val));
                } else if (int.signedness == .unsigned) {
                    l.pushUnsigned(@intCast(val));
                }
            },
            .float => |float| {
                if (float.bits == 32) {
                    l.pushNumber(@floatCast(val));
                } else if (float.bits == 64) {
                    l.pushNumber(val);
                } else {
                    @compileError(std.fmt.comptimePrint("Unsupported float type: {s}", .{@typeName(T)}));
                }
            },
            .pointer => |ptr| {
                const child_type_info = @typeInfo(ptr.child);
                const child_is_u8_sentinel_array = child_type_info == .array and
                    child_type_info.array.sentinel() == 0 and child_type_info.array.child == u8;
                if (child_is_u8_sentinel_array) {
                    l.pushString(val);
                } else if (ptr.size == .slice) {
                    l.createTable(@intCast(val.len), 0);
                    for (val, 1..) |v, i| {
                        l.pushInteger(@intCast(i));
                        l.pushVal(v);
                        l.setTable(.at(-3));
                    }
                } else {
                    @compileError(std.fmt.comptimePrint("Unsupported type: {s}", .{@typeName(T)}));
                }
            },
            .optional => |opt| {
                if (val) |v| {
                    l.pushVal(@as(opt.child, v));
                } else {
                    l.pushNil();
                }
            },
            .@"struct" => {
                l.createPushTable(val);
            },
            else => @compileError(std.fmt.comptimePrint("Unsupported type: {s}", .{@typeName(T)})),
        }
    }

    pub fn pushTable(l: *State, index: Index, val: anytype) void {
        const T = @TypeOf(val);
        const ti = @typeInfo(T);
        if (ti != .@"struct") @compileError("pushTable only works with structs, use pushVal for other types");

        if (@hasDecl(T, "luauPushTable")) {
            const customPushTable: fn (val: T, l: *State, table: Index) void = @field(T, "luauPushTable");
            customPushTable(val, l, index);
            return;
        }

        const fields = ti.@"struct".fields;
        inline for (fields) |field| {
            const field_name = field.name;
            const field_value = @field(val, field_name);
            l.pushVal(field_value);
            const old_index: i32 = @intFromEnum(index);
            // adjusting because this will push the value to the stack
            // and the index will be incremented
            const new_index: Index = @enumFromInt(if (old_index < 0) old_index - 1 else old_index);
            l.setField(new_index, field_name);
        }
    }

    pub fn createPushTable(l: *State, val: anytype) void {
        l.createTable(0, 0);
        l.pushTable(.at(-1), val);
    }
};

pub const OpenLibraries = struct {
    base: bool = true,
    coroutine: bool = true,
    table: bool = true,
    os: bool = true,
    string: bool = true,
    bit32: bool = true,
    buffer: bool = true,
    utf8: bool = true,
    math: bool = true,
    debug: bool = true,
    vector: bool = true,

    pub const none: OpenLibraries = .{
        .base = false,
        .coroutine = false,
        .table = false,
        .os = false,
        .string = false,
        .bit32 = false,
        .buffer = false,
        .utf8 = false,
        .math = false,
        .debug = false,
        .vector = false,
    };
};

pub const GcOp = union(RawGcOp) {
    stop,
    restart,
    collect,
    count,
    countb,
    is_running,
    step,
    set_goal: i32,
    set_step_mul: i32,
    set_step_size: i32,
};

pub const RawGcOp = enum(c_uint) {
    stop = 0,
    restart = 1,
    collect = 2,
    count = 3,
    countb = 4,
    is_running = 5,
    step = 6,
    set_goal = 7,
    set_step_mul = 8,
    set_step_size = 9,
};

pub const ZigFnInt = *const fn (state: *State) i32;
pub const ZigFnVoid = *const fn (state: *State) void;
pub const ZigFnErrorSet = *const fn (state: *State) anyerror!i32;

pub fn zigToCFn(comptime fn_ty: std.builtin.Type.Fn, comptime f: anytype) CFunction {
    const ri = @typeInfo(fn_ty.return_type orelse @compileError("Fn must return something"));
    switch (ri) {
        .int => |_| {
            _ = @as(ZigFnInt, f);
            return struct {
                fn inner(s: *State) callconv(.C) c_int {
                    return @call(.always_inline, f, .{s});
                }
            }.inner;
        },
        .void => |_| {
            _ = @as(ZigFnVoid, f);
            return struct {
                fn inner(s: *State) callconv(.C) c_int {
                    @call(.always_inline, f, .{s});
                    return 0;
                }
            }.inner;
        },
        .error_union => |_| {
            _ = @as(ZigFnErrorSet, f);
            return struct {
                fn inner(s: *State) callconv(.C) c_int {
                    if (@call(.always_inline, f, .{s})) |res|
                        return res
                    else |err| switch (@as(anyerror, @errorCast(err))) {
                        error.RaiseLuauError => s.err(),
                        else => s.errStr("{s}", .{@errorName(err)}),
                    }
                }
            }.inner;
        },
        else => @compileError("Unsupported Fn Return type"),
    }
}

pub fn toCFn(comptime f: anytype) CFunction {
    const t = @TypeOf(f);
    const ti = @typeInfo(t);
    switch (ti) {
        .@"fn" => |Fn| return zigToCFn(Fn, f),
        .pointer => |ptr| {
            // *const fn ...
            if (!ptr.is_const)
                @compileError("Pointer must be constant");
            const pi = @typeInfo(ptr.child);
            switch (pi) {
                .@"fn" => |Fn| return zigToCFn(Fn, f),
                else => @compileError("Pointer must be a pointer to a function"),
            }
        },
        else => @compileError("zig_fn must be a Fn or a Fn Pointer"),
    }
    @compileError("Could not determine zig_fn type");
}

comptime {
    std.testing.refAllDeclsRecursive(@This());
}

test "loading code" {
    const testing_allocator = std.testing.allocator;

    var l = try State.init(&testing_allocator);
    defer l.deinit();

    const code =
        \\return 1
    ;
    const chunkname = "test";

    var a = luau.Allocator.init(&testing_allocator);
    defer a.deinit();

    var ast_name_table = ast.NameTable.init(a);
    defer ast_name_table.deinit();

    var parse_result = ast.parse(code, ast_name_table, a);
    defer parse_result.deinit();

    const compiled = try compiler.compileParseResult(testing_allocator, parse_result, ast_name_table, null);
    defer testing_allocator.free(compiled);

    const ok = l.load(chunkname, compiled);
    try std.testing.expect(ok);

    l.call(0, 1);

    const result = l.toIntegerx(.at(-1));
    try std.testing.expectEqual(result, 1);
}

test "State.pushVal" {
    const testing_allocator = std.testing.allocator;

    var l = try State.init(&testing_allocator);
    defer l.deinit();

    const test_string = "test";
    l.pushVal(test_string);

    const result = l.toLengthString(.at(-1));
    try std.testing.expectEqualSlices(u8, test_string, result);

    const test_number: Number = 1.0;
    l.pushVal(test_number);

    const result_num = l.toNumberx(.at(-1));
    try std.testing.expectEqual(test_number, result_num);
}

test "State.pushTable" {
    const testing_allocator = std.testing.allocator;
    var l = try State.init(&testing_allocator);
    defer l.deinit();

    const code =
        \\local x = input.x
        \\local y = input.y
        \\local z = input.z
        \\local a = input.a
        \\local b = input.b
        \\local c = input.c
        \\
        \\assert(typeof(x) == "number")
        \\assert(typeof(y) == "boolean")
        \\assert(typeof(z) == "string")
        \\assert(typeof(a) == "buffer")
        \\assert(typeof(b) == "nil")
        \\assert(typeof(c) == "number")
    ;

    const input: struct {
        x: i32,
        y: bool,
    } = .{
        .x = 1,
        .y = true,
    };

    l.open(.{});
    // typed table
    l.createPushTable(input);
    // anonymous table
    l.pushTable(.at(-1), .{
        .z = "test",
        .a = Buffer.literal(&.{ 0, 10, 10, 10 }),
        .b = @as(?u8, null),
        .c = @as(?u8, 1),
    });
    l.setGlobal("input");

    const chunkname = "test";
    var a = luau.Allocator.init(&testing_allocator);
    defer a.deinit();
    var ast_name_table = ast.NameTable.init(a);
    defer ast_name_table.deinit();
    var parse_result = ast.parse(code, ast_name_table, a);
    defer parse_result.deinit();
    const compiled = try compiler.compileParseResult(testing_allocator, parse_result, ast_name_table, null);
    defer testing_allocator.free(compiled);
    const ok = l.load(chunkname, compiled);
    try std.testing.expect(ok);

    try std.testing.expectEqual(.ok, l.pcall(0, 0, .none));
}

const MyTestTable = struct {
    helloo: i32 = 1,

    pub fn luauPushTable(self: MyTestTable, l: *State, table: Index) void {
        l.pushString("test");
        l.setField(table.shift(-1), "test");
        l.pushInteger(self.helloo);
        l.setField(table.shift(-1), "renamed");
    }
};
test "State.pushTable with a custom function" {
    const testing_allocator = std.testing.allocator;
    var l = try State.init(&testing_allocator);
    defer l.deinit();

    l.open(.{});

    const test_table = MyTestTable{
        .helloo = 5,
    };
    l.createPushTable(test_table);
    l.setGlobal("input");

    const many_tables: []const MyTestTable = &.{
        MyTestTable{
            .helloo = 1,
        },
        MyTestTable{
            .helloo = 2,
        },
    };
    l.pushVal(many_tables);
    l.setGlobal("many_tables");

    const code =
        \\assert(input.test == "test")
        \\assert(input.renamed == 5)
        \\
        \\assert(typeof(many_tables) == "table")
        \\
        \\assert(many_tables[1].test == "test")
        \\assert(many_tables[1].renamed == 1)
        \\
        \\assert(many_tables[2].test == "test")
        \\assert(many_tables[2].renamed == 2)
        \\
        \\assert(many_tables[3] == nil)
    ;

    const chunkname = "test";
    var a = luau.Allocator.init(&testing_allocator);
    defer a.deinit();
    var ast_name_table = ast.NameTable.init(a);
    defer ast_name_table.deinit();
    var parse_result = ast.parse(code, ast_name_table, a);
    defer parse_result.deinit();
    const compiled = try compiler.compileParseResult(testing_allocator, parse_result, ast_name_table, null);
    defer testing_allocator.free(compiled);
    const ok = l.load(chunkname, compiled);
    try std.testing.expect(ok);

    try std.testing.expectEqual(.ok, l.pcall(0, 0, .none));
}

const std = @import("std");

const luau = @import("root.zig");
const ast = @import("ast.zig");
const compiler = @import("compiler.zig");

const raw = @import("luau_raw");
