#if defined(__wasm__)
#define LUAU_TRY_CATCH(trying, catching) zig_luau_try_catch_js(trying, catching)
#define LUAU_THROW(e) zig_luau_throw_js(e)
#define LUAU_EXTERNAL_TRY_CATCH
#endif

#include "Luau/Common.h"
#include "ldo.h"

#include <functional>
#include <cstdio>
#include <cstdlib>
#include <string>
#include <vector>

static int assertionHandler(const char* expr, const char* file, int line, const char* function)
{
    printf("%s(%d): ASSERTION FAILED: %s\n", file, line, expr);
    return 1;
}

LUA_API void zig_registerAssertionHandler()
{
    Luau::assertHandler() = assertionHandler;
}

LUA_API void zig_luau_free(void* ptr)
{
    free(ptr);
}

#if defined(__wasm__)
#if not defined(LUAU_WASM_ENV_NAME)
#define LUAU_WASM_ENV_NAME "env"
#endif
struct TryCatchContext
{
    std::function<void()> trying;
    std::function<void(const std::exception&)> catching;
};
// only clang compilers support C/C++ -> wasm so it's safe to use the attribute here
__attribute__((import_module(LUAU_WASM_ENV_NAME), import_name("try_catch"))) void zig_luau_try_catch_js_impl(TryCatchContext* context);
__attribute__((import_module(LUAU_WASM_ENV_NAME), import_name("throw"))) void zig_luau_throw_js_impl(const std::exception* e);

void zig_luau_try_catch_js(std::function<void()> trying, std::function<void(const std::exception&)> catching)
{
    auto context = TryCatchContext{trying, catching};
    zig_luau_try_catch_js_impl(&context);
}

void zig_luau_throw_js(const std::exception& e)
{
    zig_luau_throw_js_impl(&e);
}

LUA_API void zig_luau_try_impl(TryCatchContext* context)
{
    context->trying();
}

LUA_API void zig_luau_catch_impl(TryCatchContext* context, const std::exception& e)
{
    context->catching(e);
}
#endif

#include "Compiler/src/BuiltinFolding.cpp"
#include "Compiler/src/Builtins.cpp"
#include "Compiler/src/BytecodeBuilder.cpp"
#include "Compiler/src/Compiler.cpp"
#include "Compiler/src/ConstantFolding.cpp"
#include "Compiler/src/CostModel.cpp"
#include "Compiler/src/TableShape.cpp"
#include "Compiler/src/Types.cpp"
#include "Compiler/src/ValueTracking.cpp"
#include "Compiler/src/lcode.cpp"

#include "VM/src/lapi.cpp"
#include "VM/src/laux.cpp"
#include "VM/src/lbaselib.cpp"
#include "VM/src/lbitlib.cpp"
#include "VM/src/lbuffer.cpp"
#include "VM/src/lbuflib.cpp"
#include "VM/src/lbuiltins.cpp"
#include "VM/src/lcorolib.cpp"
#include "VM/src/ldblib.cpp"
#include "VM/src/ldebug.cpp"
#include "VM/src/ldo.cpp"
#include "VM/src/lfunc.cpp"
#include "VM/src/lgc.cpp"
#include "VM/src/lgcdebug.cpp"
#include "VM/src/linit.cpp"
#include "VM/src/lmathlib.cpp"
#include "VM/src/lmem.cpp"
#include "VM/src/lnumprint.cpp"
#include "VM/src/lobject.cpp"
#include "VM/src/loslib.cpp"
#include "VM/src/lperf.cpp"
#include "VM/src/lstate.cpp"
#include "VM/src/lstring.cpp"
#include "VM/src/lstrlib.cpp"
#include "VM/src/ltable.cpp"
#include "VM/src/ltablib.cpp"
#include "VM/src/ltm.cpp"
#include "VM/src/ludata.cpp"
#include "VM/src/lutf8lib.cpp"
#include "VM/src/lveclib.cpp"
#include "VM/src/lvmexecute.cpp"
#include "VM/src/lvmload.cpp"
#include "VM/src/lvmutils.cpp"

#include "Ast/src/Allocator.cpp"
#include "Ast/src/Ast.cpp"
#include "Ast/src/Confusables.cpp"
#include "Ast/src/Cst.cpp"
#include "Ast/src/Lexer.cpp"
#include "Ast/src/Location.cpp"
#include "Ast/src/Parser.cpp"
#include "Ast/src/StringUtils.cpp"
#include "Ast/src/TimeTrace.cpp"

#if defined(LUAU_CODEGEN)
#include "CodeGen/src/AssemblyBuilderA64.cpp"
#include "CodeGen/src/AssemblyBuilderX64.cpp"
#include "CodeGen/src/CodeAllocator.cpp"
#include "CodeGen/src/CodeBlockUnwind.cpp"
#include "CodeGen/src/CodeGen.cpp"
#include "CodeGen/src/CodeGenAssembly.cpp"
#include "CodeGen/src/CodeGenContext.cpp"
#include "CodeGen/src/CodeGenUtils.cpp"
#include "CodeGen/src/CodeGenA64.cpp"
#include "CodeGen/src/CodeGenX64.cpp"
#include "CodeGen/src/EmitBuiltinsX64.cpp"
#include "CodeGen/src/EmitCommonX64.cpp"
#include "CodeGen/src/EmitInstructionX64.cpp"
#include "CodeGen/src/IrAnalysis.cpp"
#include "CodeGen/src/IrBuilder.cpp"
#include "CodeGen/src/IrCallWrapperX64.cpp"
#include "CodeGen/src/IrDump.cpp"
#include "CodeGen/src/IrLoweringA64.cpp"
#include "CodeGen/src/IrLoweringX64.cpp"
#include "CodeGen/src/IrRegAllocA64.cpp"
#include "CodeGen/src/IrRegAllocX64.cpp"
#include "CodeGen/src/IrTranslateBuiltins.cpp"
#include "CodeGen/src/IrTranslation.cpp"
#include "CodeGen/src/IrUtils.cpp"
#include "CodeGen/src/IrValueLocationTracking.cpp"
#include "CodeGen/src/lcodegen.cpp"
#include "CodeGen/src/NativeProtoExecData.cpp"
#include "CodeGen/src/NativeState.cpp"
#include "CodeGen/src/OptimizeConstProp.cpp"
#include "CodeGen/src/OptimizeDeadStore.cpp"
#include "CodeGen/src/OptimizeFinalX64.cpp"
#include "CodeGen/src/UnwindBuilderDwarf2.cpp"
#include "CodeGen/src/UnwindBuilderWin.cpp"
#include "CodeGen/src/BytecodeAnalysis.cpp"
#include "CodeGen/src/BytecodeSummary.cpp"
#include "CodeGen/src/SharedCodeAllocator.cpp"
#endif