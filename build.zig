const std = @import("std");

const compile_commands = @import("compile_commands.zig");

const LUAU_VERSION: std.SemanticVersion = .{ .major = 0, .minor = 662, .patch = 0 };

pub fn build(b: *std.Build) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{ .preferred_optimize_mode = .ReleaseFast });

    const use_4_vector = b.option(bool, "use 4 vector", "Build Luau to use 4-vectors instead of the default 3-vector.") orelse false;
    const wasm_env_name = b.option([]const u8, "wasm_env", "The environment to import symbols from when building for WebAssembly. Defaults to `env`") orelse "env";
    const build_shared = b.option(bool, "build_shared", "Build Luau as a shared library.") orelse false;

    const is_wasm = target.result.ofmt == .wasm;
    const has_code_gen = !is_wasm;

    if (build_shared and is_wasm) {
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
    if (target.result.os.tag != .wasi)
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
    if (has_code_gen) {
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

    const cpp_version = "c++17";
    const FLAGS = [_][]const u8{
        // setjmp.h compile error in Wasm
        "-DLUA_USE_LONGJMP=" ++ if (!is_wasm) "1" else "0",
        b.fmt("-DLUA_API={s}", .{api}),
        b.fmt("-DLUACODE_API={s}", .{api}),
        b.fmt("-DLUACODEGEN_API={s}", .{api}),
        if (use_4_vector) "-DLUA_VECTOR_SIZE=4" else "",
        if (is_wasm) "-fexceptions" else "",
        if (is_wasm) b.fmt("-DLUAU_WASM_ENV_NAME=\"{s}\"", .{wasm_env_name}) else "",
        "-std=" ++ cpp_version,
    };
    lib.linkLibC();
    lib.linkLibCpp();
    lib.addCSourceFile(.{ .file = b.path("src/luau.cpp"), .flags = &FLAGS });
    lib.addCSourceFile(.{ .file = b.path("src/Ast.cpp"), .flags = &FLAGS });
    lib.addCSourceFile(.{ .file = b.path("src/Compiler.cpp"), .flags = &FLAGS });

    lib.installHeader(b.path("VM/include/lua.h"), "lua.h");
    lib.installHeader(b.path("VM/include/lualib.h"), "lualib.h");
    lib.installHeader(b.path("VM/include/luaconf.h"), "luaconf.h");
    if (has_code_gen)
        lib.installHeader(b.path("CodeGen/include/luacodegen.h"), "luacodegen.h");

    lib.installHeader(b.path("Compiler/include/luacode.h"), "luacode.h");

    const step = compile_commands.createStep(b, "cdb", &.{lib});
    b.getInstallStep().dependOn(step);

    const mod = b.addModule("luau", .{
        .target = target,
        .optimize = optimize,
        .root_source_file = b.path("src/root.zig"),
    });
    mod.linkLibrary(lib);
    mod.sanitize_c = false;

    const mod_test = b.addTest(.{
        .root_module = mod,
    });

    const run_tests = b.addRunArtifact(mod_test);
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_tests.step);
}

pub const wasm_module_export_symbols: []const []const u8 = &.{
    "zig_luau_try_impl",
    "zig_luau_catch_impl",
};
