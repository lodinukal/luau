#include "luau_prelude.h"
#include "Luau/Common.h"
#include <cstdio>

static int assertionHandler(const char* expr, const char* file, int line, const char* function)
{
    printf("%s(%d): ASSERTION FAILED: %s\n", file, line, expr);
    return 1;
}

LUA_API void registerAssertionHandler()
{
    Luau::assertHandler() = assertionHandler;
}
