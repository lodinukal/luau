const std = @import("std");

const LUAU_VERSION: std.SemanticVersion = .{ .major = 0, .minor = 661, .patch = 0 };

pub fn build(b: *std.Build) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{ .preferred_optimize_mode = .ReleaseFast });

    const use_4_vector = b.option(bool, "use 4 vector", "Build Luau to use 4-vectors instead of the default 3-vector.") orelse false;
    const wasm_env_name = b.option([]const u8, "wasm_env", "The environment to import symbols from when building for WebAssembly. Defaults to `env`") orelse "env";
    const build_shared = b.option(bool, "build_shared", "Build Luau as a shared library.") orelse false;

    if (build_shared and target.result.isWasm()) {
        std.debug.panic("Building Luau as a shared library is not supported on WebAssembly.", .{});
        return;
    }

    const config = b.addOptions();
    config.addOption(bool, "use_4_vector", use_4_vector);
    config.addOption(std.SemanticVersion, "luau_version", LUAU_VERSION);

    const headers = b.addTranslateC(.{
        .root_source_file = b.path("src/luau.h"),
        .target = target,
        .optimize = optimize,
    });
    headers.addIncludePath(b.path("Compiler/include"));
    headers.addIncludePath(b.path("VM/include"));
    if (!target.result.isWasm())
        headers.addIncludePath(b.path("CodeGen/include"));
    _ = headers.addModule("luau");

    const lib = if (build_shared) b.addSharedLibrary(.{
        .name = "luau",
        .target = target,
        .optimize = optimize,
        .version = LUAU_VERSION,
    }) else b.addStaticLibrary(.{
        .name = "luau",
        .target = target,
        .optimize = optimize,
        .version = LUAU_VERSION,
    });
    b.installArtifact(lib);
    lib.root_module.pic = true;
    lib.addIncludePath(b.path("src/Lib"));
    lib.addIncludePath(b.path("Ast/include"));
    lib.addIncludePath(b.path("Common/include"));
    lib.addIncludePath(b.path("Compiler/include"));
    // CodeGen is not supported on WASM
    if (!target.result.isWasm()) {
        lib.addIncludePath(b.path("CodeGen/include"));
    }
    lib.addIncludePath(b.path("VM/include"));
    lib.addIncludePath(b.path("VM/src"));
    lib.addIncludePath(b.path(""));

    const api = api: {
        if (!build_shared) break :api "extern \"C\"";
        switch (target.result.os.tag) {
            .windows => break :api "extern \"C\" __declspec(dllexport)",
            else => break :api "extern \"C\"",
        }
    };

    const FLAGS = [_][]const u8{
        // setjmp.h compile error in Wasm
        "-DLUA_USE_LONGJMP=" ++ if (!target.result.isWasm()) "1" else "0",
        b.fmt("-DLUA_API={s}", .{api}),
        b.fmt("-DLUACODE_API={s}", .{api}),
        b.fmt("-DLUACODEGEN_API={s}", .{api}),
        if (use_4_vector) "-DLUA_VECTOR_SIZE=4" else "",
        if (target.result.isWasm()) "-fexceptions" else "",
        if (target.result.isWasm()) b.fmt("-DLUAU_WASM_ENV_NAME=\"{s}\"", .{wasm_env_name}) else "",
    };
    lib.linkLibCpp();
    lib.addCSourceFile(.{ .file = b.path("src/luau.cpp"), .flags = &FLAGS });

    lib.installHeader(b.path("VM/include/lua.h"), "lua.h");
    lib.installHeader(b.path("VM/include/lualib.h"), "lualib.h");
    lib.installHeader(b.path("VM/include/luaconf.h"), "luaconf.h");
    if (!target.result.isWasm())
        lib.installHeader(b.path("CodeGen/include/luacodegen.h"), "luacodegen.h");
}

pub const wasm_module_export_symbols: []const []const u8 = &.{
    "zig_luau_try_impl",
    "zig_luau_catch_impl",
};
