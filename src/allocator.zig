const alignment = 8;

pub const CVTable = extern struct {
    allocate: *const fn (*anyopaque, usize) callconv(.c) ?*align(alignment) anyopaque,
    deallocate: *const fn (*anyopaque, ?*align(alignment) anyopaque) callconv(.c) void,

    fn zigAllocate(context: *anyopaque, size: usize) callconv(.c) ?*align(alignment) anyopaque {
        const as_allocator: *std.mem.Allocator = @ptrCast(@alignCast(context));
        const bytes = as_allocator.alignedAlloc(u8, 8, size + 8) catch return null;
        std.mem.writeInt(usize, bytes[0..@sizeOf(usize)], size, .little);
        return (bytes[8..]).ptr;
    }

    fn zigDeallocate(context: *anyopaque, ptr: ?*align(alignment) anyopaque) callconv(.c) void {
        const as_allocator: *std.mem.Allocator = @ptrCast(@alignCast(context));
        if (ptr) |p| {
            var bytes_shifted: [*:0]align(alignment) u8 = @ptrCast(p);
            bytes_shifted -= 8;
            const size = std.mem.readInt(usize, bytes_shifted[0..@sizeOf(usize)], .little);
            as_allocator.free(bytes_shifted[0 .. size + 8]);
        }
    }
};

pub const vtable: CVTable = .{
    .allocate = CVTable.zigAllocate,
    .deallocate = CVTable.zigDeallocate,
};

extern fn Luau_Ast_Allocator_new(ctx: *anyopaque, vtable: CVTable) callconv(.c) *Allocator;
extern fn Luau_Ast_Allocator_default() callconv(.c) *Allocator;
extern fn Luau_Ast_Allocator_free(*Allocator) callconv(.c) void;

pub const Allocator = opaque {
    // a bit hacky but ok
    pub inline fn init(allocator: *const std.mem.Allocator) *Allocator {
        const ctx: *anyopaque = @constCast(@ptrCast(allocator));
        return Luau_Ast_Allocator_new(ctx, vtable);
    }

    pub inline fn default() *Allocator {
        return Luau_Ast_Allocator_default();
    }

    pub inline fn deinit(allocator: *Allocator) void {
        Luau_Ast_Allocator_free(allocator);
    }
};

test Allocator {
    var allocator = std.testing.allocator;
    const a = Allocator.init(&allocator);
    defer a.deinit();
}

const std = @import("std");

comptime {
    std.testing.refAllDecls(@This());
}
