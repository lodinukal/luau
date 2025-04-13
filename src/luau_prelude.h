#pragma once
#include "luaconf.h"

#if defined(__wasm__)
#include <functional>

#define LUAU_TRY_CATCH(trying, catching) zig_luau_try_catch_js(trying, catching)
#define LUAU_THROW(e) zig_luau_throw_js(e)
#define LUAU_EXTERNAL_TRY_CATCH

#if not defined(LUAU_WASM_ENV_NAME)
#define LUAU_WASM_ENV_NAME "env"
#endif

struct TryCatchContext
{
    std::function<void()> trying;
    std::function<void(const std::exception&)> catching;
};
// only clang compilers support C/C++ -> wasm so it's safe to use the attribute here
__attribute__((import_module(LUAU_WASM_ENV_NAME), import_name("try_catch"))) void luau_try_catch_js_impl(TryCatchContext* context);
__attribute__((import_module(LUAU_WASM_ENV_NAME), import_name("throw"))) void luau_throw_js_impl(const std::exception* e);

void zig_luau_try_catch_js(std::function<void()> trying, std::function<void(const std::exception&)> catching);
void zig_luau_throw_js(const std::exception& e);
LUA_API void luau_try_impl(TryCatchContext* context);
LUA_API void luau_catch_impl(TryCatchContext* context, const std::exception& e);
#endif