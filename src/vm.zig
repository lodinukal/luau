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
    pub const vector_size: usize = 3;

    pub const extra_size = vector_size - 2;
};

pub const Index = enum(i32) {
    multiret = -1,

    registry = -config.max_cstack - 2000,
    environ = -config.max_cstack - 2001,
    globals = -config.max_cstack - 2002,

    pub fn at(i: i32) Index {
        return @enumFromInt(i);
    }

    pub inline fn upvalue(i: i32) i32 {
        return @enumFromInt(@intFromEnum(Index.globals) - i);
    }

    pub inline fn isPseudo(self: Index) bool {
        return @intFromEnum(self) <= @intFromEnum(Index.registry);
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

pub const CFunction = *const fn (l: *State) Index;
pub const Continuation = *const fn (l: *State, status: Status) Index;

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

pub const Atom = enum(i32) { _ };

pub const State = opaque {
    extern fn lua_newstate(alloc_f: ?Alloc, userdata: ?*anyopaque) callconv(.c) *State;
    extern fn lua_close(l: *State) callconv(.c) void;
    extern fn lua_newthread(l: *State) callconv(.c) *State;
    extern fn lua_mainthread(l: *State) callconv(.c) *State;
    extern fn lua_resetthread(l: *State) callconv(.c) void;
    extern fn lua_isthreadreset(l: *State) callconv(.c) bool;

    pub inline fn init(allocator: *const std.mem.Allocator) *State {
        return lua_newstate(alloc, @constCast(allocator));
    }

    pub inline fn deinit(l: *State) void {
        lua_close(l);
    }

    /// Creates a new thread, pushes it on the stack, and returns a pointer to a lua_State that
    /// represents this new thread. The new state returned by this function shares with the original
    /// state all global objects (such as tables), but has an independent execution stack.
    ///
    /// There is no explicit function to close or to destroy a thread. Threads are subject to garbage
    /// collection, like any Lua object.
    pub inline fn initThread(l: *State) *State {
        return lua_newthread(l);
    }

    pub inline fn mainThread(l: *State) *State {
        return lua_mainthread(l);
    }

    pub inline fn resetThread(l: *State) void {
        lua_resetthread(l);
    }

    pub inline fn isThreadReset(l: *State) bool {
        return lua_isthreadreset(l);
    }

    // stack manipulation
    extern fn lua_absindex(l: *State, idx: Index) callconv(.c) Index;
    extern fn lua_gettop(l: *State) callconv(.c) Index;
    extern fn lua_settop(l: *State, idx: Index) callconv(.c) void;
    extern fn lua_remove(l: *State, idx: Index) callconv(.c) void;
    extern fn lua_insert(l: *State, idx: Index) callconv(.c) void;
    extern fn lua_replace(l: *State, idx: Index) callconv(.c) void;
    extern fn lua_checkstack(l: *State, size: i32) callconv(.c) bool;
    extern fn lua_rawcheckstack(l: *State, size: i32) callconv(.c) void;
    extern fn lua_xmove(from: *State, to: *State, n: i32) callconv(.c) void;
    extern fn lua_xpush(from: *State, to: *State, n: Index) callconv(.c) void;

    /// Converts the acceptable index idx into an equivalent absolute index (that is,
    /// one that does not depend on the stack top).
    pub inline fn absIndex(l: *State, idx: Index) Index {
        return lua_absindex(l, idx);
    }

    /// Returns the index of the top element in the stack. Because indices start at 1,
    /// this result is equal to the number of elements in the stack; in particular, 0 means an empty stack.
    pub inline fn getTop(l: *State) Index {
        return lua_gettop(l);
    }

    /// Accepts any acceptable index, or 0, and sets the stack top to this index. If the new top is larger
    /// than the old one, then the new elements are filled with nil. If index is 0, then all stack elements
    /// are removed.
    pub inline fn setTop(l: *State, idx: Index) void {
        return lua_settop(l, idx);
    }

    /// Removes the element at the given valid index, shifting down the elements above this index to fill
    /// the gap. Cannot be called with a pseudo-index, because a pseudo-index is not an actual stack position.
    pub inline fn remove(l: *State, idx: Index) void {
        return lua_remove(l, idx);
    }

    /// Moves the top element into the given valid index, shifting up the elements above this index to open space.
    /// Cannot be called with a pseudo-index, because a pseudo-index is not an actual stack position.
    pub inline fn insert(l: *State, idx: Index) void {
        return lua_insert(l, idx);
    }

    /// Moves the top element into the given position (and pops it), without shifting any element
    /// (therefore replacing the value at the given position).
    pub inline fn replace(l: *State, idx: Index) void {
        return lua_replace(l, idx);
    }

    /// Ensures that there are at least extra free stack slots in the stack. It returns false if
    /// it cannot grow the stack to that size. This function never shrinks the stack; if the stack
    /// is already larger than the new size, it is left unchanged.
    pub inline fn checkStack(l: *State, size: i32) bool {
        return lua_checkstack(l, size);
    }

    /// like checkStack but allows for unlimited stack frames
    pub inline fn rawCheckStack(l: *State, size: i32) void {
        return lua_rawcheckstack(l, size);
    }

    /// Exchange values between different threads of the same global state.
    /// This function pops n values from the stack from, and pushes them onto the stack to.
    pub inline fn xmove(from: *State, to: *State, n: i32) void {
        return lua_xmove(from, to, n);
    }

    /// see xmove, but doesn't pop the values from the stack
    pub inline fn xpush(from: *State, to: *State, n: Index) void {
        return lua_xpush(from, to, n);
    }

    // access functions
    extern fn lua_isnumber(l: *State, idx: Index) callconv(.c) bool;
    extern fn lua_isstring(l: *State, idx: Index) callconv(.c) bool;
    extern fn lua_iscfunction(l: *State, idx: Index) callconv(.c) bool;
    extern fn lua_isLfunction(l: *State, idx: Index) callconv(.c) bool;
    extern fn lua_isuserdata(l: *State, idx: Index) callconv(.c) bool;
    extern fn lua_type(l: *State, idx: Index) callconv(.c) Type;
    extern fn lua_typename(l: *State, tp: Type) callconv(.c) [*:0]const u8;

    extern fn lua_equal(l: *State, a: Index, b: Index) callconv(.c) bool;
    extern fn lua_rawequal(l: *State, a: Index, b: Index) callconv(.c) bool;
    extern fn lua_lessthan(l: *State, a: Index, b: Index) callconv(.c) bool;

    extern fn lua_tonumberx(l: *State, idx: Index, isnum: ?*bool) callconv(.c) Number;
    extern fn lua_tointegerx(l: *State, idx: Index, isnum: ?*bool) callconv(.c) Integer;
    extern fn lua_tounsignedx(l: *State, idx: Index, isnum: ?*bool) callconv(.c) Unsigned;
    extern fn lua_tovector(l: *State, idx: Index) callconv(.c) *[config.vector_size]Number;
    extern fn lua_toboolean(l: *State, idx: Index) callconv(.c) bool;
    extern fn lua_tolstring(l: *State, idx: Index, len: ?*usize) callconv(.c) [*:0]const u8;
    extern fn lua_tostringatom(l: *State, idx: Index, atom: ?*i32) callconv(.c) [*:0]const u8;
    extern fn lua_tolstringatom(l: *State, idx: Index, len: ?*usize, atom: ?*i32) callconv(.c) [*:0]const u8;
    extern fn lua_namecallatom(l: *State, atom: ?*i32) callconv(.c) [*:0]const u8;
    extern fn lua_objlen(l: *State, idx: Index) callconv(.c) i32;
    extern fn lua_tocfunction(l: *State, idx: Index) callconv(.c) CFunction;
    extern fn lua_tolightuserdata(l: *State, idx: Index) callconv(.c) *anyopaque;
    extern fn lua_tolightuserdatatagged(l: *State, idx: Index, tag: i32) callconv(.c) *anyopaque;
    extern fn lua_touserdata(l: *State, idx: Index) callconv(.c) *anyopaque;
    extern fn lua_touserdatatagged(l: *State, idx: Index, tag: i32) callconv(.c) *anyopaque;
    extern fn lua_userdatatag(l: *State, idx: Index) callconv(.c) i32;
    extern fn lua_lightuserdatatag(l: *State, idx: Index) callconv(.c) i32;
    extern fn lua_tothread(l: *State, idx: Index) callconv(.c) *State;
    extern fn lua_tobuffer(l: *State, idx: Index, len: ?*usize) callconv(.c) [*]const u8;
    extern fn lua_topointer(l: *State, idx: Index) callconv(.c) [*]const u8;

    pub inline fn isNumber(l: *State, idx: Index) bool {
        return lua_isnumber(l, idx);
    }

    pub inline fn isString(l: *State, idx: Index) bool {
        return lua_isstring(l, idx);
    }

    pub inline fn isCFunction(l: *State, idx: Index) bool {
        return lua_iscfunction(l, idx);
    }

    pub inline fn isLFunction(l: *State, idx: Index) bool {
        return lua_isLfunction(l, idx);
    }

    pub inline fn isUserdata(l: *State, idx: Index) bool {
        return lua_isuserdata(l, idx);
    }

    pub inline fn @"type"(l: *State, idx: Index) Type {
        return lua_type(l, idx);
    }

    pub inline fn typeName(l: *State, tp: Type) [*:0]const u8 {
        return lua_typename(l, tp);
    }

    pub inline fn equal(l: *State, a: Index, b: Index) bool {
        return lua_equal(l, a, b);
    }

    pub inline fn rawequal(l: *State, a: Index, b: Index) bool {
        return lua_rawequal(l, a, b);
    }

    pub inline fn lessThan(l: *State, a: Index, b: Index) bool {
        return lua_lessthan(l, a, b);
    }

    pub inline fn toNumberx(l: *State, idx: Index, isnum: ?*bool) Number {
        return lua_tonumberx(l, idx, isnum);
    }

    pub inline fn toIntegerx(l: *State, idx: Index, isnum: ?*bool) Integer {
        return lua_tointegerx(l, idx, isnum);
    }

    pub inline fn toUnsignedx(l: *State, idx: Index, isnum: ?*bool) Unsigned {
        return lua_tounsignedx(l, idx, isnum);
    }

    pub inline fn toVector(l: *State, idx: Index) *[config.vector_size]Number {
        return lua_tovector(l, idx);
    }

    pub inline fn toBoolean(l: *State, idx: Index) bool {
        return lua_toboolean(l, idx);
    }

    pub inline fn toLengthString(l: *State, idx: Index) []const u8 {
        var len: usize = 0;
        return lua_tolstring(l, idx, &len)[0..len];
    }

    pub inline fn toStringAtom(l: *State, idx: Index, atom: ?*Atom) [*:0]const u8 {
        return lua_tostringatom(l, idx, atom);
    }

    pub inline fn toLengthStringAtom(l: *State, idx: Index, atom: ?*Atom) [*:0]const u8 {
        var len: usize = 0;
        return lua_tolstringatom(l, idx, &len, atom)[0..len];
    }

    pub inline fn nameCallAtom(l: *State, atom: ?*Atom) [*:0]const u8 {
        return lua_namecallatom(l, atom);
    }

    pub inline fn objLen(l: *State, idx: Index) i32 {
        return lua_objlen(l, idx);
    }

    pub inline fn toCFunction(l: *State, idx: Index) CFunction {
        return lua_tocfunction(l, idx);
    }

    pub inline fn toLightUserdata(l: *State, idx: Index) *anyopaque {
        return lua_tolightuserdata(l, idx);
    }

    pub inline fn toLightUserdataTagged(l: *State, idx: Index, tag: i32) *anyopaque {
        return lua_tolightuserdatatagged(l, idx, tag);
    }

    pub inline fn toUserdata(l: *State, idx: Index) *anyopaque {
        return lua_touserdata(l, idx);
    }

    pub inline fn toUserdataTagged(l: *State, idx: Index, tag: i32) *anyopaque {
        return lua_touserdatatagged(l, idx, tag);
    }

    pub inline fn userdataTag(l: *State, idx: Index) i32 {
        return lua_userdatatag(l, idx);
    }

    pub inline fn lightUserdataTag(l: *State, idx: Index) i32 {
        return lua_lightuserdatatag(l, idx);
    }

    pub inline fn toThread(l: *State, idx: Index) *State {
        return lua_tothread(l, idx);
    }

    pub inline fn toBuffer(l: *State, idx: Index) [*]const u8 {
        var len: usize = 0;
        return lua_tobuffer(l, idx, &len)[0..len];
    }

    pub inline fn toPointer(l: *State, idx: Index) [*]const u8 {
        return lua_topointer(l, idx);
    }

    // push functions
    extern fn lua_pushnil(l: *State) callconv(.c) void;
    extern fn lua_pushnumber(l: *State, n: Number) callconv(.c) void;
    extern fn lua_pushinteger(l: *State, n: Integer) callconv(.c) void;
    extern fn lua_pushunsigned(l: *State, n: Unsigned) callconv(.c) void;
    pub const size_4 = struct {
        extern fn lua_pushvector(l: *State, x: Number, y: Number, z: Number, w: Number) callconv(.c) void;
    };
    pub const size_3 = struct {
        extern fn lua_pushvector(l: *State, x: Number, y: Number, z: Number) callconv(.c) void;
    };
    extern fn lua_pushlstring(l: *State, s: [*:0]const u8, l: usize) callconv(.c) void;
    extern fn lua_pushstring(l: *State, s: [*:0]const u8) callconv(.c) void;
    extern fn lua_pushclosurek(l: *State, f: CFunction, debug_name: [*:0]const u8, nup: i32, cont: Continuation) callconv(.c) void;
    extern fn lua_pushboolean(l: *State, b: bool) callconv(.c) void;
    // true if main thread
    extern fn lua_pushthread(l: *State) callconv(.c) bool;

    extern fn lua_pushlightuserdatatagged(l: *State, p: *anyopaque, tag: i32) callconv(.c) void;
    extern fn lua_newuserdatatagged(l: *State, sz: usize, tag: i32) callconv(.c) *anyopaque;
    extern fn lua_newuserdatataggedwithmetatable(l: *State, sz: usize, tag: i32) callconv(.c) *anyopaque;
    extern fn lua_newuserdatadtor(l: *State, sz: usize, dtor: *fn (ptr: *anyopaque) void) callconv(.c) *anyopaque;

    extern fn lua_newbuffer(l: *State, sz: usize) callconv(.c) [*]u8;

    pub inline fn pushNil(l: *State) void {
        return lua_pushnil(l);
    }

    pub inline fn pushNumber(l: *State, n: Number) void {
        return lua_pushnumber(l, n);
    }

    pub inline fn pushInteger(l: *State, n: Integer) void {
        return lua_pushinteger(l, n);
    }

    pub inline fn pushUnsigned(l: *State, n: Unsigned) void {
        return lua_pushunsigned(l, n);
    }

    pub inline fn pushVector(l: *State, vector: [config.vector_size]Number) void {
        if (config.vector_size == 4) {
            return size_4.lua_pushvector(l, vector[0], vector[1], vector[2], vector[3]);
        } else {
            return size_3.lua_pushvector(l, vector[0], vector[1], vector[2]);
        }
    }

    pub inline fn pushLengthString(l: *State, s: []const u8) void {
        return lua_pushlstring(l, s, @intCast(s.len));
    }

    pub inline fn pushString(l: *State, s: [*:0]const u8) void {
        return lua_pushstring(l, s);
    }

    pub inline fn pushClosurek(l: *State, f: CFunction, debug_name: [*:0]const u8, upvalues: i32, cont: Continuation) void {
        return lua_pushclosurek(l, f, debug_name, upvalues, cont);
    }

    pub inline fn pushBoolean(l: *State, b: bool) void {
        return lua_pushboolean(l, b);
    }

    /// returns true if the main thread
    pub inline fn pushThread(l: *State) bool {
        return lua_pushthread(l);
    }

    pub inline fn pushLightUserdataTagged(l: *State, p: *anyopaque, tag: i32) void {
        return lua_pushlightuserdatatagged(l, p, tag);
    }

    pub inline fn newUserdataTagged(l: *State, sz: usize, tag: i32) *anyopaque {
        return lua_newuserdatatagged(l, sz, tag);
    }

    pub inline fn newUserdataTaggedWithMetatable(l: *State, sz: usize, tag: i32) *anyopaque {
        return lua_newuserdatataggedwithmetatable(l, sz, tag);
    }

    pub inline fn newUserdataDtor(l: *State, sz: usize, dtor: *fn (ptr: *anyopaque) void) *anyopaque {
        return lua_newuserdatadtor(l, sz, dtor);
    }

    pub inline fn newBuffer(l: *State, sz: usize) []u8 {
        return lua_newbuffer(l, sz)[0..sz];
    }

    // get functions
    extern fn lua_gettable(l: *State, idx: Index) callconv(.c) void;
    extern fn lua_getfield(l: *State, idx: Index, k: [*:0]const u8) callconv(.c) void;
    extern fn lua_rawgetfield(l: *State, idx: Index, k: [*:0]const u8) callconv(.c) void;
    extern fn lua_rawget(l: *State, idx: Index) callconv(.c) void;
    extern fn lua_rawgeti(l: *State, idx: Index, n: i32) callconv(.c) void;
    extern fn lua_createtable(l: *State, narr: i32, nrec: i32) callconv(.c) void;

    extern fn lua_setreadonly(l: *State, idx: Index, enabled: i32) callconv(.c) void;
    extern fn lua_getreadonly(l: *State, idx: Index) callconv(.c) i32;
    extern fn lua_setsafeenv(l: *State, idx: Index, enabled: bool) callconv(.c) void;

    extern fn lua_getmetatable(l: *State, objindex: Index) callconv(.c) i32;
    extern fn lua_getfenv(l: *State, idx: Index) callconv(.c) void;

    pub inline fn getTable(l: *State, idx: Index) void {
        return lua_gettable(l, idx);
    }

    pub inline fn getField(l: *State, idx: Index, k: [:0]const u8) void {
        return lua_getfield(l, idx, k.ptr);
    }

    pub inline fn rawGetField(l: *State, idx: Index, k: [:0]const u8) void {
        return lua_rawgetfield(l, idx, k);
    }

    pub inline fn rawGet(l: *State, idx: Index) void {
        return lua_rawget(l, idx);
    }

    pub inline fn rawGeti(l: *State, idx: Index, n: i32) void {
        return lua_rawgeti(l, idx, n);
    }

    pub inline fn createTable(l: *State, narr: i32, nrec: i32) void {
        return lua_createtable(l, narr, nrec);
    }

    pub inline fn setReadonly(l: *State, idx: Index, enabled: bool) void {
        return lua_setreadonly(l, idx, if (enabled) 1 else 0);
    }

    pub inline fn getReadonly(l: *State, idx: Index) bool {
        return lua_getreadonly(l, idx) != 0;
    }

    pub inline fn setSafeEnv(l: *State, idx: Index, enabled: bool) void {
        return lua_setsafeenv(l, idx, enabled);
    }

    pub inline fn getMetatable(l: *State, objindex: Index) i32 {
        return lua_getmetatable(l, objindex);
    }

    pub inline fn getFenv(l: *State, idx: Index) void {
        return lua_getfenv(l, idx);
    }

    // set functions
    extern fn lua_settable(l: *State, idx: Index) callconv(.c) void;
    extern fn lua_setfield(l: *State, idx: Index, k: [*:0]const u8) callconv(.c) void;
    extern fn lua_rawsetfield(l: *State, idx: Index, k: [*:0]const u8) callconv(.c) void;
    extern fn lua_rawset(l: *State, idx: Index) callconv(.c) void;
    extern fn lua_rawseti(l: *State, idx: Index, n: i32) callconv(.c) void;
    extern fn lua_setmetatable(l: *State, objindex: Index) callconv(.c) i32;
    extern fn lua_setfenv(l: *State, idx: Index) callconv(.c) i32;

    pub inline fn setTable(l: *State, idx: Index) void {
        return lua_settable(l, idx);
    }

    pub inline fn setField(l: *State, idx: Index, k: [:0]const u8) void {
        return lua_setfield(l, idx, k);
    }

    pub inline fn rawSetField(l: *State, idx: Index, k: [:0]const u8) void {
        return lua_rawsetfield(l, idx, k);
    }

    pub inline fn rawSet(l: *State, idx: Index) void {
        return lua_rawset(l, idx);
    }

    pub inline fn rawSeti(l: *State, idx: Index, n: i32) void {
        return lua_rawseti(l, idx, n);
    }

    pub inline fn setMetatable(l: *State, objindex: Index) i32 {
        return lua_setmetatable(l, objindex);
    }

    pub inline fn setFenv(l: *State, idx: Index) i32 {
        return lua_setfenv(l, idx);
    }

    // load and call functions
    extern fn luau_load(l: *State, chunkname: [*:0]const u8, data: [*]const u8, size: usize, env: i32) callconv(.c) i32;
    extern fn lua_call(l: *State, nargs: i32, nresults: i32) callconv(.c) void;
    extern fn lua_pcall(l: *State, nargs: i32, nresults: i32, errfunc: i32) callconv(.c) i32;

    pub inline fn loadEnv(l: *State, chunkname: [:0]const u8, data: []const u8, env: i32) i32 {
        return luau_load(l, chunkname.ptr, data.ptr, data.len, env);
    }

    pub inline fn load(l: *State, chunkname: [:0]const u8, data: []const u8) i32 {
        return l.loadEnv(chunkname, data, 0);
    }

    pub inline fn call(l: *State, nargs: i32, nresults: i32) void {
        return lua_call(l, nargs, nresults);
    }

    pub inline fn pcall(l: *State, nargs: i32, nresults: i32, errfunc: i32) i32 {
        return lua_pcall(l, nargs, nresults, errfunc);
    }

    // coroutine functions
    extern fn lua_yield(l: *State, nresults: i32) callconv(.c) i32;
    extern fn lua_break(l: *State) callconv(.c) i32;
    extern fn lua_resume(l: *State, from: *State, narg: i32) callconv(.c) i32;
    extern fn lua_resumeerror(l: *State, from: *State) callconv(.c) i32;
    extern fn lua_status(l: *State) callconv(.c) i32;
    extern fn lua_isyieldable(l: *State) callconv(.c) i32;
    extern fn lua_getthreaddata(l: *State) callconv(.c) *anyopaque;
    extern fn lua_setthreaddata(l: *State, data: *anyopaque) callconv(.c) void;
    extern fn lua_costatus(l: *State, co: *State) callconv(.c) CoroutineStatus;

    pub inline fn yield(l: *State, nresults: i32) i32 {
        return lua_yield(l, nresults);
    }

    pub inline fn @"break"(l: *State) i32 {
        return lua_break(l);
    }

    pub inline fn @"resume"(l: *State, from: *State, narg: i32) i32 {
        return lua_resume(l, from, narg);
    }

    pub inline fn resumeError(l: *State, from: *State) i32 {
        return lua_resumeerror(l, from);
    }

    pub inline fn status(l: *State) Status {
        return @enumFromInt(lua_status(l));
    }

    pub inline fn isYieldable(l: *State) bool {
        return lua_isyieldable(l) == 1;
    }

    pub inline fn getThreadData(l: *State) *anyopaque {
        return lua_getthreaddata(l);
    }

    pub inline fn setThreadData(l: *State, data: *anyopaque) void {
        return lua_setthreaddata(l, data);
    }

    pub inline fn coStatus(l: *State, co: *State) CoroutineStatus {
        return lua_costatus(l, co);
    }
};

comptime {
    std.testing.refAllDeclsRecursive(@This());
}

test "loading code" {
    const testing_allocator = std.testing.allocator;

    var l = State.init(&testing_allocator);
    defer l.deinit();

    const code =
        \\return 1
    ;
    const chunkname = "test";

    var a = ast.Allocator.init(&testing_allocator);
    defer a.deinit();

    var ast_name_table = ast.NameTable.init(a);
    defer ast_name_table.deinit();

    var parse_result = ast.parse(code, ast_name_table, a);
    defer parse_result.deinit();

    const compiled = try compiler.compileParseResult(testing_allocator, parse_result, ast_name_table, null);
    defer testing_allocator.free(compiled);

    const err = l.load(chunkname, compiled);
    try std.testing.expect(err == 0);

    l.call(0, 1);

    const result = l.toIntegerx(.at(-1), null);
    try std.testing.expect(result == 1);
}

const std = @import("std");

const ast = @import("ast.zig");
const compiler = @import("compiler.zig");
