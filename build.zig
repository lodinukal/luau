const std = @import("std");

const compile_commands = @import("compile_commands.zig");

const LUAU_VERSION: std.SemanticVersion = .{ .major = 0, .minor = 674, .patch = 0 };

pub fn build(b: *std.Build) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const use_4_vector = b.option(bool, "use 4 vector", "Build Luau to use 4-vectors instead of the default 3-vector.") orelse false;
    const wasm_env_name = b.option([]const u8, "wasm_env", "The environment to import symbols from when building for WebAssembly. Defaults to `env`") orelse "env";
    const build_shared = b.option(bool, "build_shared", "Build Luau as a shared library.") orelse false;

    const enable_analysis = b.option(bool, "enable_analysis", "Enable the analysis module (very big).") orelse false;

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
    const luau_raw = headers.addModule("luau_raw");

    const lib = b.addLibrary(.{
        .name = "luau",
        .root_module = b.createModule(.{
            .target = target,
            .optimize = optimize,
        }),
        .version = LUAU_VERSION,
        .linkage = if (build_shared) .dynamic else .static,
    });
    b.installArtifact(lib);
    lib.root_module.pic = true;
    lib.addIncludePath(b.path("src/"));
    lib.addIncludePath(b.path("Ast/include"));
    lib.addIncludePath(b.path("Common/include"));
    lib.addIncludePath(b.path("Compiler/include"));
    // // CodeGen is not supported on WASM
    if (has_code_gen) {
        lib.addIncludePath(b.path("CodeGen/include"));
    }
    lib.addIncludePath(b.path("VM/include"));
    lib.addIncludePath(b.path("VM/src"));
    lib.addIncludePath(b.path("Require/Navigator/include"));
    lib.addIncludePath(b.path("Require/Runtime/include"));
    if (enable_analysis) {
        lib.addIncludePath(b.path("EqSat/include"));
        lib.addIncludePath(b.path("Analysis/include"));
    }
    lib.addIncludePath(b.path("Config/include"));

    const api: ?[]const u8 = api: {
        if (!build_shared) break :api "extern \"C\"";
        switch (target.result.os.tag) {
            .windows => break :api "extern \"C\" __declspec(dllexport)",
            else => break :api "extern \"C\"",
        }
    };

    const cpp_version = "c++17";

    var flags: std.ArrayList([]const u8) = .init(b.allocator);
    if (api) |got_api| {
        try flags.appendSlice(&.{
            b.fmt("-DLUA_API={s}", .{got_api}),
            b.fmt("-DLUACODE_API={s}", .{got_api}),
            b.fmt("-DLUACODEGEN_API={s}", .{got_api}),
        });
    }
    try flags.appendSlice(&.{
        // "-DLUA_USE_LONGJMP=" ++ if (!is_wasm) "1" else "0",
        "-DLUA_USE_LONGJMP=" ++ "0",
        "-std=" ++ cpp_version,
    });

    if (is_wasm) {
        try flags.appendSlice(&.{
            "-fexceptions",
            b.fmt("-DLUAU_WASM_ENV_NAME=\"{s}\"", .{wasm_env_name}),
        });
        lib.addCSourceFile(.{
            .file = b.path("src/luau_web_eh.cpp"),
            .language = .cpp,
            .flags = flags.items,
        });
    }

    lib.linkLibC();
    lib.linkLibCpp();
    lib.root_module.addCMacro("LUAU_HAS_PRELUDE", "");
    lib.addCSourceFiles(.{
        .files = ast_sources,
        .language = .cpp,
        .flags = flags.items,
        .root = b.path("Ast/src/"),
    });
    lib.addCSourceFiles(.{
        .files = compiler_sources,
        .language = .cpp,
        .flags = flags.items,
        .root = b.path("Compiler/src"),
    });
    if (enable_analysis) {
        lib.addCSourceFiles(.{
            .files = analysis_sources,
            .language = .cpp,
            .flags = flags.items,
            .root = b.path("Analysis/src"),
        });
        lib.addCSourceFiles(.{
            .files = eqsat_sources,
            .language = .cpp,
            .flags = flags.items,
            .root = b.path("EqSat/src"),
        });
    }
    lib.addCSourceFiles(.{
        .files = config_sources,
        .language = .cpp,
        .flags = flags.items,
        .root = b.path("Config/src"),
    });
    lib.addCSourceFiles(.{
        .files = require_navigator_sources,
        .language = .cpp,
        .flags = flags.items,
        .root = b.path("Require/Navigator/src"),
    });
    lib.addCSourceFiles(.{
        .files = require_runtime_sources,
        .language = .cpp,
        .flags = flags.items,
        .root = b.path("Require/Runtime/src"),
    });

    if (has_code_gen) {
        lib.addCSourceFiles(.{
            .files = codegen_sources,
            .language = .cpp,
            .flags = flags.items,
            .root = b.path("CodeGen/src"),
        });
    }

    lib.addCSourceFiles(.{
        .files = vm_sources,
        .language = .cpp,
        .flags = flags.items,
        .root = b.path("VM/src"),
    });

    lib.installHeadersDirectory(b.path("Analysis/include"), "", .{});
    lib.installHeadersDirectory(b.path("Ast/include"), "", .{});
    lib.installHeadersDirectory(b.path("CLI/include"), "", .{});
    if (has_code_gen) {
        lib.installHeadersDirectory(b.path("CodeGen/include"), "", .{});
    }
    lib.installHeadersDirectory(b.path("Common/include"), "", .{});
    lib.installHeadersDirectory(b.path("Compiler/include"), "", .{});
    lib.installHeadersDirectory(b.path("Config/include"), "", .{});
    lib.installHeadersDirectory(b.path("EqSat/include"), "", .{});
    lib.installHeadersDirectory(b.path("Require/Navigator/include"), "", .{});
    lib.installHeadersDirectory(b.path("Require/Runtime/include"), "", .{});
    lib.installHeadersDirectory(b.path("VM/include"), "", .{});

    {
        lib.installHeader(b.path("VM/include/lua.h"), "lua.h");
        lib.installHeader(b.path("VM/include/lualib.h"), "lualib.h");
        lib.installHeader(b.path("VM/include/luaconf.h"), "luaconf.h");
        if (has_code_gen)
            lib.installHeader(b.path("CodeGen/include/luacodegen.h"), "luacodegen.h");
        lib.installHeader(b.path("Compiler/include/luacode.h"), "luacode.h");
    }

    const step = compile_commands.createStep(b, "cdb", &.{lib});
    b.getInstallStep().dependOn(step);

    const mod = b.addModule("luau", .{
        .target = target,
        .optimize = optimize,
        .root_source_file = b.path("src/root.zig"),
    });
    mod.pic = true;
    mod.linkLibrary(lib);
    mod.addImport("luau_raw", luau_raw);
    mod.addCSourceFiles(.{
        .files = &.{
            "Ast.cpp",
            "Compiler.cpp",
            "Assert.cpp",
            "VM.cpp",
        },
        .flags = flags.items,
        .language = .cpp,
        .root = b.path("src/"),
    });
    mod.addIncludePath(b.path("src/"));

    const mod_test = b.addTest(.{
        .root_module = mod,
    });

    const run_tests = b.addRunArtifact(mod_test);
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_tests.step);
}

pub const wasm_module_export_symbols: []const []const u8 = &.{
    "luau_try_impl",
    "luau_catch_impl",
};

const ast_sources: []const []const u8 = &.{
    "Allocator.cpp",
    "Ast.cpp",
    "Confusables.cpp",
    "Cst.cpp",
    "Lexer.cpp",
    "Location.cpp",
    "Parser.cpp",
    "StringUtils.cpp",
    "TimeTrace.cpp",
};

const compiler_sources: []const []const u8 = &.{
    "BytecodeBuilder.cpp",
    "Compiler.cpp",
    "Builtins.cpp",
    "BuiltinFolding.cpp",
    "ConstantFolding.cpp",
    "CostModel.cpp",
    "TableShape.cpp",
    "Types.cpp",
    "ValueTracking.cpp",
    "lcode.cpp",
};

const codegen_sources: []const []const u8 = &.{
    "AssemblyBuilderA64.cpp",
    "AssemblyBuilderX64.cpp",
    "CodeAllocator.cpp",
    "CodeBlockUnwind.cpp",
    "CodeGen.cpp",
    "CodeGenAssembly.cpp",
    "CodeGenContext.cpp",
    "CodeGenUtils.cpp",
    "CodeGenA64.cpp",
    "CodeGenX64.cpp",
    "EmitBuiltinsX64.cpp",
    "EmitCommonX64.cpp",
    "EmitInstructionX64.cpp",
    "IrAnalysis.cpp",
    "IrBuilder.cpp",
    "IrCallWrapperX64.cpp",
    "IrDump.cpp",
    "IrLoweringA64.cpp",
    "IrLoweringX64.cpp",
    "IrRegAllocA64.cpp",
    "IrRegAllocX64.cpp",
    "IrTranslateBuiltins.cpp",
    "IrTranslation.cpp",
    "IrUtils.cpp",
    "IrValueLocationTracking.cpp",
    "lcodegen.cpp",
    "NativeProtoExecData.cpp",
    "NativeState.cpp",
    "OptimizeConstProp.cpp",
    "OptimizeDeadStore.cpp",
    "OptimizeFinalX64.cpp",
    "UnwindBuilderDwarf2.cpp",
    "UnwindBuilderWin.cpp",
    "BytecodeAnalysis.cpp",
    "BytecodeSummary.cpp",
    "SharedCodeAllocator.cpp",
};

const vm_sources: []const []const u8 = &.{
    "lapi.cpp",
    "laux.cpp",
    "lbaselib.cpp",
    "lbitlib.cpp",
    "lbuffer.cpp",
    "lbuflib.cpp",
    "lbuiltins.cpp",
    "lcorolib.cpp",
    "ldblib.cpp",
    "ldebug.cpp",
    "ldo.cpp",
    "lfunc.cpp",
    "lgc.cpp",
    "lgcdebug.cpp",
    "linit.cpp",
    "lmathlib.cpp",
    "lmem.cpp",
    "lnumprint.cpp",
    "lobject.cpp",
    "loslib.cpp",
    "lperf.cpp",
    "lstate.cpp",
    "lstring.cpp",
    "lstrlib.cpp",
    "ltable.cpp",
    "ltablib.cpp",
    "ltm.cpp",
    "ludata.cpp",
    "lutf8lib.cpp",
    "lveclib.cpp",
    "lvmexecute.cpp",
    "lvmload.cpp",
    "lvmutils.cpp",
};

const analysis_sources: []const []const u8 = &.{
    "Anyification.cpp",
    "ApplyTypeFunction.cpp",
    "AstJsonEncoder.cpp",
    "AstQuery.cpp",
    "Autocomplete.cpp",
    "AutocompleteCore.cpp",
    "BuiltinDefinitions.cpp",
    "Clone.cpp",
    "Constraint.cpp",
    "ConstraintGenerator.cpp",
    "ConstraintSolver.cpp",
    "DataFlowGraph.cpp",
    "DcrLogger.cpp",
    "Def.cpp",
    "Differ.cpp",
    "EmbeddedBuiltinDefinitions.cpp",
    "Error.cpp",
    "EqSatSimplification.cpp",
    "FileResolver.cpp",
    "FragmentAutocomplete.cpp",
    "Frontend.cpp",
    "Generalization.cpp",
    "GlobalTypes.cpp",
    "InferPolarity.cpp",
    "Instantiation.cpp",
    "Instantiation2.cpp",
    "IostreamHelpers.cpp",
    "JsonEmitter.cpp",
    "Linter.cpp",
    "LValue.cpp",
    "Module.cpp",
    "NonStrictTypeChecker.cpp",
    "Normalize.cpp",
    "OverloadResolution.cpp",
    "Quantify.cpp",
    "Refinement.cpp",
    "RequireTracer.cpp",
    "Scope.cpp",
    "Simplify.cpp",
    "Substitution.cpp",
    "Subtyping.cpp",
    "Symbol.cpp",
    "TableLiteralInference.cpp",
    "ToDot.cpp",
    "TopoSortStatements.cpp",
    "ToString.cpp",
    "Transpiler.cpp",
    "TxnLog.cpp",
    "Type.cpp",
    "TypeArena.cpp",
    "TypeAttach.cpp",
    "TypeChecker2.cpp",
    "TypedAllocator.cpp",
    "TypeFunction.cpp",
    "TypeFunctionReductionGuesser.cpp",
    "TypeFunctionRuntime.cpp",
    "TypeFunctionRuntimeBuilder.cpp",
    "TypeInfer.cpp",
    "TypeOrPack.cpp",
    "TypePack.cpp",
    "TypePath.cpp",
    "TypeUtils.cpp",
    "Unifiable.cpp",
    "Unifier.cpp",
    "Unifier2.cpp",
};

const config_sources: []const []const u8 = &.{
    "LinterConfig.cpp",
    "Config.cpp",
};

const eqsat_sources: []const []const u8 = &.{
    "Id.cpp",
    "UnionFind.cpp",
};

const require_navigator_sources: []const []const u8 = &.{
    "PathUtilities.cpp",
    "RequireNavigator.cpp",
};

const require_runtime_sources: []const []const u8 = &.{
    "Navigation.cpp",
    "Require.cpp",
    "RequireImpl.cpp",
};
