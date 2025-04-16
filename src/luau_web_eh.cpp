#include "luau_prelude.h"

#if defined(__wasm__)
void luau_try_catch_js(std::function<void()> trying, std::function<void(const std::exception&)> catching)
{
    auto context = TryCatchContext{trying, catching};
    luau_try_catch_js_impl(&context);
}

void luau_throw_js(const std::exception& e)
{
    luau_throw_js_impl(&e);
}

LUA_API void luau_try_impl(TryCatchContext* context)
{
    context->trying();
}

LUA_API void luau_catch_impl(TryCatchContext* context, const std::exception& e)
{
    context->catching(e);
}
#endif