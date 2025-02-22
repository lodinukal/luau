const ast = @import("ast.zig");
const compiler = @import("compiler.zig");
const vm = @import("vm.zig");

comptime {
    _ = ast;
    _ = compiler;
    _ = vm;
}
