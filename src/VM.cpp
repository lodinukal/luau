
#include "lualib.h"

#include "lstate.h"
#include "lapi.h"
#include "ldo.h"
#include "ludata.h"

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>

#define ZIG_LUAU_VM(name) (Luau_VM_##name)

LUAU_FASTFLAG(LuauYieldableContinuations)

LUA_API void ZIG_LUAU_VM(enable_yieldable_continuations)(bool enable)
{
    FFlag::LuauYieldableContinuations.value = enable;
}