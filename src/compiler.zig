const std = @import("std");
const luau = @import("root.zig");
const ast = @import("ast.zig");

pub const c = @cImport({
    @cInclude("luacode.h");
});

extern "c" fn Luau_Compiler_compile_ParseResult(*const ast.ParseResult, *const ast.NameTable, *usize, ?*c.lua_CompileOptions, ?*anyopaque) ?[*]const u8;
extern "c" fn Luau_Compiler_compile_free(*anyopaque) void;

pub const CompileOptions = struct {
    optimization_level: i32 = 1,
    debug_level: i32 = 1,
    coverage_level: i32 = 0,
    /// global builtin to construct vectors; disabled by default (<vector_lib>.<vector_ctor>)
    vector_lib: ?[*:0]const u8 = null,
    vector_ctor: ?[*:0]const u8 = null,
    /// vector type name for type tables; disabled by default
    vector_type: ?[*:0]const u8 = null,
    /// null-terminated array of globals that are mutable; disables the import optimization for fields accessed through these
    mutable_globals: ?[*:null]const ?[*:0]const u8 = null,
};

pub fn compileParseResult(
    allocator: std.mem.Allocator,
    parse_result: *ast.ParseResult,
    name_table: *ast.NameTable,
    options: ?CompileOptions,
) error{OutOfMemory}![]const u8 {
    var size: usize = 0;
    var opts = if (options) |o| c.lua_CompileOptions{
        .optimizationLevel = o.optimization_level,
        .debugLevel = o.debug_level,
        .coverageLevel = o.coverage_level,
        .vectorLib = o.vector_lib,
        .vectorCtor = o.vector_ctor,
        .mutableGlobals = o.mutable_globals,
    } else null;
    const bytes = Luau_Compiler_compile_ParseResult(
        parse_result,
        name_table,
        &size,
        if (opts) |*o| o else null,
        null,
    ) orelse return error.OutOfMemory;
    defer Luau_Compiler_compile_free(@ptrCast(@constCast(bytes)));
    return try allocator.dupe(u8, bytes[0..size]);
}

test compileParseResult {
    var allocator = std.testing.allocator;

    const Allocator = luau.Allocator;

    var a = Allocator.init(&allocator);
    defer a.deinit();

    var astNameTable = ast.NameTable.init(a);
    defer astNameTable.deinit();
    const source =
        \\--!test
        \\-- This is a test comment
        \\local x =
        \\
    ;

    var parse_result = ast.parse(source, astNameTable, a);
    defer parse_result.deinit();

    const bytes = try compileParseResult(allocator, parse_result, astNameTable, null);
    defer allocator.free(bytes);

    try std.testing.expect(bytes[0] == 0);
    try std.testing.expectEqualStrings(bytes[1..], ":4: Expected identifier when parsing expression, got <eof>");
}

/// when freeing this, free raw_bytes
pub const CompileResult = struct {
    ok: bool,
    bytes: []const u8,
    raw_bytes: []const u8,

    pub fn deinit(self: CompileResult, allocator: std.mem.Allocator) void {
        allocator.free(self.raw_bytes);
    }
};

pub fn compile(
    allocator: std.mem.Allocator,
    temp_allocator: std.mem.Allocator,
    source: []const u8,
    options: CompileOptions,
) error{OutOfMemory}!CompileResult {
    const const_ref_temp_allocator = temp_allocator;
    var luau_allocator = luau.Allocator.init(&const_ref_temp_allocator);
    defer luau_allocator.deinit();

    var name_table = ast.NameTable.init(luau_allocator);
    defer name_table.deinit();

    var parse_result = ast.parse(source, name_table, luau_allocator);
    defer parse_result.deinit();

    const bytes = try compileParseResult(allocator, parse_result, name_table, options);
    // means there was a compile error
    if (bytes[0] == 0) {
        return .{
            .ok = false,
            .bytes = bytes[1..],
            .raw_bytes = bytes,
        };
    }
    return .{
        .ok = true,
        .bytes = bytes,
        .raw_bytes = bytes,
    };
}

comptime {
    std.testing.refAllDecls(@This());
}
