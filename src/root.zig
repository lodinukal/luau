pub const Allocator = @import("allocator.zig").Allocator;
pub const ast = @import("ast.zig");
pub const compiler = @import("compiler.zig");
pub const vm = @import("vm.zig");
pub const util = @import("util.zig");
pub const require = @import("require.zig");

comptime {
    _ = Allocator;
    _ = ast;
    _ = compiler;
    _ = vm;
    _ = util;
    _ = require;
}

pub const State = vm.State;

pub const CompileOptions = compiler.CompileOptions;
pub const CompileResult = compiler.CompileResult;
pub const compile = compiler.compile;

pub const marshal = util.marshal;
