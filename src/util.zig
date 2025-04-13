pub fn marshal(comptime T: type, l: *luau.State, value: T) !i32 {
    const type_info = @typeInfo(T);
    switch (type_info) {
        .int => |int| switch (int.signedness) {
            .signed => {
                l.pushInteger(@truncate(value));
                return 1;
            },
            .unsigned => {
                l.pushUnsigned(@truncate(value));
                return 1;
            },
        },
        .comptime_int => {
            l.pushInteger(@truncate(value));
            return 1;
        },
        .float, .comptime_float => {
            l.pushNumber(@floatCast(value));
            return 1;
        },
        .bool => {
            l.pushBoolean(value);
            return 1;
        },
        .void => {
            return 0;
        },
        .vector => |vector| {
            if (vector.len != luau.vm.config.vector_size) @compileError(std.fmt.comptimePrint(
                "marshalling vector to luau, expected len {}, got {}",
                .{
                    luau.vm.config.vector_size,
                    vector.len,
                },
            ));
            const child_type_info = @typeInfo(vector.child);
            switch (child_type_info) {
                .float => l.pushVector(@floatCast(value)),
                .int => l.pushVector(@floatFromInt(value)),
                else => @compileError(std.fmt.comptimePrint(
                    "marshalling vector to luau, invalid child type, expected int or float, got {}",
                    .{@typeName(vector.child)},
                )),
            }
            return 1;
        },
        .null, .undefined => {
            l.pushNil();
            return 1;
        },
        .@"enum" => {
            l.pushLengthString(@tagName(value));
            return 1;
        },
        .pointer => |pointer| {
            switch (pointer.size) {
                .one => {
                    l.pushLightUserdata(@ptrCast(value));
                },
                .slice => {
                    if (pointer.child == u8) {
                        l.pushLengthString(value);
                    } else {
                        @compileError(std.fmt.comptimePrint("marshalling slice pointer of not bytes not allowed", .{}));
                    }
                },
                else => @compileError(std.fmt.comptimePrint("marshalling non single or slice pointer not allowed", .{})),
            }
            return 1;
        },
        .optional => |optional| {
            if (value) |v| {
                return try marshal(optional.child, l, v);
            }
            l.pushNil();
            return 1;
        },
        .@"struct" => |structure| {
            if (structure.is_tuple) {
                var total: i32 = 0;
                for (structure.fields) |field| {
                    const name = field.name;
                    total += try marshal(field.type, l, @field(value, name));
                }
                return @intCast(total);
            }

            if (@hasDecl(T, "marshalLuau")) {
                return value.marshalLuau(l);
            }

            l.createTable(0, @intCast(structure.fields.len));
            const table_at = l.absIndex(.at(1));

            inline for (structure.fields) |field| {
                const name = field.name;
                const values = try marshal(field.type, l, @field(value, name));
                if (values > 0) {
                    l.setField(table_at, name);
                    // above has already popped one off
                    l.pop(values - 1);
                }
            }

            return 1;
        },
        else => {
            @compileError(std.fmt.comptimePrint(
                "marshalling {} to lua is not allowed",
                .{@typeName(T)},
            ));
        },
    }
}

test marshal {
    const allocator = std.testing.allocator;
    var arena: std.heap.ArenaAllocator = .init(allocator);
    defer arena.deinit();
    const temp_allocator = arena.allocator();

    const SOURCE =
        \\local employee = getEmployee()
        \\assert(employee.name == "Jacky")
        \\assert(employee.age == 17)
        \\assert(employee.is_hardworking == true)
        \\assert(employee.gender == "male")
        \\assert(employee.start_working_age == 10)
        \\assert(employee.retiring_age == nil)
        \\assert(typeof(employee.preference) == "vector")
        \\assert(employee.preference.x == 3)
        \\assert(employee.preference.y == 3)
        \\assert(employee.preference.z == 3)
    ;

    const bc = try luau.compile(allocator, temp_allocator, SOURCE, .{});
    defer bc.deinit(allocator);

    if (!bc.ok) {
        std.debug.panic("error compiling marshal test: {s}", .{bc.bytes});
    }

    var state = try luau.State.init(&allocator);
    defer state.deinit();

    state.open(.{});

    state.pushFunction(Employee.lGetEmployee, "getEmployee");
    state.setGlobal("getEmployee");

    if (!state.load("@module", bc.bytes)) {
        std.debug.panic("error loading marshal test byteocde", .{});
    }

    const status = state.pcall(0, 0, .none);
    if (status != .ok) {
        const str = state.toLengthString(.at(-1));
        std.log.err("{s}", .{str});
        return error.Fail;
    }
}

pub fn unmarshal(comptime T: type, l: *luau.State, index: luau.vm.Index) !T {
    const type_info = @typeInfo(T);
    switch (type_info) {
        .int => |int| switch (int.signedness) {
            .signed => return @intCast(l.toIntegerx(index) orelse return error.InvalidInt),
            .unsigned => return @intCast(l.toUnsignedx(index) orelse return error.InvalidInt),
        },
        .comptime_int => return @intCast(l.toIntegerx(index) orelse return error.InvalidInt),
        .float, .comptime_float => return @floatCast(l.toNumberx(index) orelse return error.InvalidFloat),
        .bool => return l.toBoolean(index),
        .void => return void,
        .vector => |vector| {
            if (vector.len != luau.vm.config.vector_size) @compileError(std.fmt.comptimePrint(
                "unmarshalling vector from luau, expected len {}, got {}",
                .{
                    luau.vm.config.vector_size,
                    vector.len,
                },
            ));
            const child_type_info = @typeInfo(vector.child);
            switch (child_type_info) {
                .float => return @floatCast(l.toVector(index)),
                .int => return @intCast(l.toVector(index)),
                else => @compileError(std.fmt.comptimePrint(
                    "unmarshalling vector from luau, invalid child type, expected int or float, got {}",
                    .{@typeName(vector.child)},
                )),
            }
        },
        .null, .undefined => return void,
        .@"enum" => return std.meta.stringToEnum(T, l.toLengthString(index)) orelse return error.InvalidEnum,
        .pointer => |pointer| {
            switch (pointer.size) {
                .one => return @ptrCast(l.toLightUserdata(index)),
                .slice => {
                    if (pointer.child == u8) {
                        return l.toLengthString(index);
                    } else {
                        @compileError(std.fmt.comptimePrint("unmarshalling slice pointer of not bytes not allowed", .{}));
                    }
                },
                else => @compileError(std.fmt.comptimePrint("unmarshalling non single or slice pointer not allowed", .{})),
            }
        },
        .optional => |optional| {
            if (l.isNil(index)) {
                return null;
            }
            return unmarshal(optional.child, l, index);
        },
        .@"struct" => |structure| {
            if (structure.is_tuple) {
                var value: T = undefined;
                for (structure.fields) |field| {
                    const name = field.name;
                    const field_index = l.getField(index, name);
                    if (field_index == 0) {
                        @compileError(std.fmt.comptimePrint("unmarshalling struct from luau, field {} not found", .{name}));
                    }
                    const field_value = try unmarshal(field.type, l, field_index);
                    @field(value, name) = field_value;
                    l.pop(1);
                }
                return value;
            }

            if (@hasDecl(T, "unmarshalLuau")) {
                return T.unmarshalLuau(l, index);
            }

            const table_at = l.absIndex(index);
            var value: T = undefined;
            inline for (structure.fields) |field| {
                const name = field.name;
                const field_index = l.getField(table_at, name);
                if (field_index == 0) {
                    @compileError(std.fmt.comptimePrint("unmarshalling struct from luau, field {} not found", .{name}));
                }
                const field_value = unmarshal(field.type, l, field_index);
                @field(value, name) = field_value;
                l.pop(1);
            }
            return value;
        },
        else => {
            @compileError(std.fmt.comptimePrint(
                "unmarshalling {} from lua is not allowed",
                .{@typeName(T)},
            ));
        },
    }
}

const Employee = struct {
    name: []const u8,
    age: i32,
    is_hardworking: bool,
    gender: enum {
        male,
        female,
        non_binary,
    },
    start_working_age: ?i32,
    retiring_age: ?i32,
    preference: luau.vm.Vector,

    fn lGetEmployee(l: *luau.State) !i32 {
        const value: @This() = .{
            .name = "Jacky",
            .age = 17,
            .is_hardworking = true,
            .gender = .male,
            .start_working_age = 10,
            .retiring_age = null,
            .preference = @splat(3.0),
        };
        _ = try marshal(Employee, l, value);
        return 1;
    }
};

const luau = @import("root.zig");

const std = @import("std");
