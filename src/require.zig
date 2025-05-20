pub const luau = @import("root.zig");

pub const NavigateResult = enum(u32) {
    success,
    ambiguous,
    not_found,
};

/// Functions returning WRITE_SUCCESS are expected to set their size_out argument
/// to the number of bytes written to the buffer. If WRITE_BUFFER_TOO_SMALL is
/// returned, size_out should be set to the required buffer size.
pub const WriteResult = enum(u32) {
    success,
    buffer_too_small,
    failure,
};

pub const Configuration = extern struct {
    /// Returns whether requires are permitted from the given chunkname.
    is_require_allowed: *const fn (l: *luau.State, context: *anyopaque, requirer_chunkname: [*:0]const u8) callconv(.c) bool,
    /// Resets the internal state to point at the requirer module.
    reset: *const fn (l: *luau.State, context: *anyopaque, requirer_chunkname: [*:0]const u8) callconv(.c) NavigateResult,
    /// Resets the internal state to point at an aliased module, given its exact
    /// path from a configuration file. This function is only called when an
    /// alias's path cannot be resolved relative to its configuration file.
    jump_to_alias: *const fn (l: *luau.State, context: *anyopaque, path: [*:0]const u8) callconv(.c) NavigateResult,
    /// Navigates through the context by making mutations to the internal state.
    to_parent: *const fn (l: *luau.State, context: *anyopaque) callconv(.c) NavigateResult,
    to_child: *const fn (l: *luau.State, context: *anyopaque, name: [*:0]const u8) callconv(.c) NavigateResult,
    /// Returns whether the context is currently pointing at a module.
    is_module_present: *const fn (l: *luau.State, context: *anyopaque) callconv(.c) bool,
    /// Provides a chunkname for the current module. This will be accessible
    /// through the debug library. This function is only called if
    /// is_module_present returns true.
    get_chunkname: *const fn (
        l: *luau.State,
        context: *anyopaque,
        buffer: [*:0]u8,
        buffer_size: usize,
        size_out: *usize,
    ) callconv(.c) WriteResult,
    // Provides a loadname that identifies the current module and is passed to
    // load. This function is only called if is_module_present returns true.
    get_loadname: *const fn (
        l: *luau.State,
        context: *anyopaque,
        buffer: [*:0]u8,
        buffer_size: usize,
        size_out: *usize,
    ) callconv(.c) WriteResult,
    /// Provides a cache key representing the current module. This function is
    /// only called if is_module_present returns true.
    get_cache_key: *const fn (
        l: *luau.State,
        context: *anyopaque,
        buffer: [*:0]u8,
        buffer_size: usize,
        size_out: *usize,
    ) callconv(.c) WriteResult,
    /// Returns whether a configuration file is present in the current context.
    /// If not, require-by-string will call to_parent until either a
    /// configuration file is present or NAVIGATE_FAILURE is returned (at root).
    is_config_present: *const fn (l: *luau.State, context: *anyopaque) callconv(.c) bool,
    // Parses the configuration file in the current context for the given alias
    // and returns its value or WRITE_FAILURE if not found. This function is
    // only called if is_config_present returns true. If this function pointer
    // is set, get_config must not be set. Opting in to this function pointer
    // disables parsing configuration files internally and can be used for finer
    // control over the configuration file parsing process.
    get_alias: *const fn (
        l: *luau.State,
        context: *anyopaque,
        alias: [*:0]const u8,
        buffer: [*:0]u8,
        buffer_size: usize,
        size_out: *usize,
    ) callconv(.c) WriteResult,
    /// Provides the contents of the configuration file in the current context.
    /// This function is only called if is_config_present returns true. If this
    // function pointer is set, get_alias must not be set. Opting in to this
    // function pointer enables parsing configuration files internally.
    get_config: *const fn (
        l: *luau.State,
        context: *anyopaque,
        buffer: [*:0]u8,
        buffer_size: usize,
        size_out: *usize,
    ) callconv(.c) WriteResult,
    /// Executes the module and places the result on the stack. Returns the
    /// number of results placed on the stack.
    load: *const fn (
        l: *luau.State,
        context: *anyopaque,
        path: [*:0]const u8,
        chunkname: [*:0]const u8,
        loadname: [*:0]const u8,
    ) callconv(.c) i32,
};

/// Populates function pointers in the given Configuration.
pub const ConfigurationInitFn = *const fn (config: *Configuration) callconv(.c) void;

// Initializes and pushes the require closure onto the stack without
// registration.
extern fn luarequire_pushrequire(
    l: *luau.State,
    config_init: ConfigurationInitFn,
    context: *anyopaque,
) callconv(.c) void;

/// lua_pushrequire but sets global require
extern fn luaopen_require(
    l: *luau.State,
    config_init: ConfigurationInitFn,
    context: *anyopaque,
) callconv(.c) void;

// Initializes and pushes a "proxyrequire" closure onto the stack. This function
// takes two parameters: the string path to resolve and the chunkname of an
// existing module. The path is resolved as if it were being required from the
// module that the chunkname represents.
extern fn luarequire_pushproxyrequire(
    l: *luau.State,
    config_init: ConfigurationInitFn,
    context: *anyopaque,
) callconv(.c) void;

// Registers an aliased require path to a result. After registration, the given
// result will always be immediately returned when the given path is required.
// Expects the path and table to be passed as arguments on the stack.
extern fn luarequire_registermodule(l: *luau.State) callconv(.c) void;

// Clears the entry associated with the given cache key from the require cache.
// Expects the cache key to be passed as an argument on the stack.
extern fn luarequire_clearcacheentry(l: *luau.State) callconv(.c) void;

// Clears all entries from the require cache.
extern fn luarequire_clearcache(l: *luau.State) callconv(.c) void;

pub fn pushrequire(
    l: *luau.State,
    config_init: ConfigurationInitFn,
    context: *anyopaque,
) void {
    luarequire_pushrequire(l, config_init, context);
}

pub fn open(
    l: *luau.State,
    config_init: ConfigurationInitFn,
    context: *anyopaque,
) void {
    luaopen_require(l, config_init, context);
}

pub fn pushproxyrequire(
    l: *luau.State,
    config_init: ConfigurationInitFn,
    context: *anyopaque,
) void {
    luarequire_pushproxyrequire(l, config_init, context);
}

pub fn registermodule(l: *luau.State) void {
    luarequire_registermodule(l);
}

pub fn clearcacheentry(l: *luau.State) void {
    luarequire_clearcacheentry(l);
}

pub fn clearcache(l: *luau.State) void {
    luarequire_clearcache(l);
}

comptime {
    std.testing.refAllDecls(@This());
}

const std = @import("std");
