
#include "lualib.h"
#include "Luau/Common.h"

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>

#define ZIG_LUAU_VM(name) (Luau_VM_##name)

LUAU_FASTFLAG(LuauYieldableContinuations)

LUA_API void ZIG_LUAU_VM(enable_yieldable_continuations)(bool enable)
{
    FFlag::LuauYieldableContinuations.value = enable;
}