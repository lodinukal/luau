// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/Require.h"

#include "RequireImpl.h"

#include "lua.h"
#include "lualib.h"

static void validateConfig(lua_State* L, const luarequire_Configuration& config)
{
    if (!config.is_require_allowed)
        luaL_error(L, "require configuration is missing required function pointer: is_require_allowed");
    if (!config.reset)
        luaL_error(L, "require configuration is missing required function pointer: reset");
    if (!config.jump_to_alias)
        luaL_error(L, "require configuration is missing required function pointer: jump_to_alias");
    if (!config.to_parent)
        luaL_error(L, "require configuration is missing required function pointer: to_parent");
    if (!config.to_child)
        luaL_error(L, "require configuration is missing required function pointer: to_child");
    if (!config.is_module_present)
        luaL_error(L, "require configuration is missing required function pointer: is_module_present");
    if (!config.get_contents)
        luaL_error(L, "require configuration is missing required function pointer: get_contents");
    if (!config.get_chunkname)
        luaL_error(L, "require configuration is missing required function pointer: get_chunkname");
    if (!config.get_cache_key)
        luaL_error(L, "require configuration is missing required function pointer: get_cache_key");
    if (!config.is_config_present)
        luaL_error(L, "require configuration is missing required function pointer: is_config_present");
    if (!config.get_config)
        luaL_error(L, "require configuration is missing required function pointer: get_config");
    if (!config.load)
        luaL_error(L, "require configuration is missing required function pointer: load");
}

void luaopen_require(lua_State* L, luarequire_Configuration_init config_init, void* ctx)
{
    luarequire_Configuration* config = static_cast<luarequire_Configuration*>(lua_newuserdata(L, sizeof(luarequire_Configuration)));
    if (!config)
        luaL_error(L, "failed to allocate memory for require configuration");

    config_init(config);
    validateConfig(L, *config);

    lua_pushlightuserdata(L, ctx);

    // "require" captures config and ctx as upvalues
    lua_pushcclosure(L, Luau::Require::lua_require, "require", 2);
    lua_setglobal(L, "require");
}
