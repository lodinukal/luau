const std = @import("std");

const ALIGNMENT = 8;

pub const CVTable = extern struct {
    allocate: *const fn (*anyopaque, usize) callconv(.c) ?*align(ALIGNMENT) anyopaque,
    deallocate: *const fn (*anyopaque, ?*align(ALIGNMENT) anyopaque) callconv(.c) void,

    fn zigAllocate(context: *anyopaque, size: usize) callconv(.c) ?*align(ALIGNMENT) anyopaque {
        const as_allocator: *std.mem.Allocator = @ptrCast(@alignCast(context));
        const bytes = as_allocator.alignedAlloc(u8, 8, size + 8) catch return null;
        std.mem.writeInt(usize, bytes[0..@sizeOf(usize)], size, .little);
        return (bytes[8..]).ptr;
        // return null;
    }

    fn zigDeallocate(context: *anyopaque, ptr: ?*align(ALIGNMENT) anyopaque) callconv(.c) void {
        const as_allocator: *std.mem.Allocator = @ptrCast(@alignCast(context));
        if (ptr) |p| {
            var bytes_shifted: [*:0]align(ALIGNMENT) u8 = @ptrCast(p);
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

extern "c" fn Luau_Ast_Lexer_AstNameTable_new(*Allocator) *NameTable;
extern "c" fn Luau_Ast_Lexer_AstNameTable_free(*NameTable) void;

pub const NameTable = opaque {
    pub inline fn init(allocator: *Allocator) *NameTable {
        return Luau_Ast_Lexer_AstNameTable_new(allocator);
    }

    pub inline fn deinit(self: *NameTable) void {
        Luau_Ast_Lexer_AstNameTable_free(self);
    }
};

test NameTable {
    var allocator = std.testing.allocator;
    var a = Allocator.init(&allocator);
    defer a.deinit();

    var table = NameTable.init(a);
    defer table.deinit();
}

const zig_Position = extern struct {
    line: c_uint,
    column: c_uint,
};

const zig_Location = extern struct {
    begin: zig_Position,
    end: zig_Position,
};

const zig_ParseResult_HotComment = extern struct {
    header: c_int,
    location: zig_Location,
    content: [*]const u8,
    contentLen: usize,
};

const zig_ParseResult_HotComments = extern struct {
    values: [*]zig_ParseResult_HotComment,
    size: usize,
};

const zig_ParseResult_Error = extern struct {
    location: zig_Location,
    message: [*]const u8,
    message_len: usize,
};

const zig_ParseResult_Errors = extern struct {
    values: [*]zig_ParseResult_Error,
    size: usize,
};

extern fn Luau_Ast_Parser_parse([*]const u8, usize, *NameTable, *Allocator) callconv(.c) *ParseResult;
extern fn Luau_Ast_ParseResult_free(*ParseResult) callconv(.c) void;
extern fn Luau_Ast_ParseResult_get_root(*ParseResult) callconv(.c) *Node;
extern fn Luau_Ast_ParseResult_get_hotcomments(*ParseResult) callconv(.c) zig_ParseResult_HotComments;
extern fn Luau_Ast_ParseResult_free_hotcomments(zig_ParseResult_HotComments) callconv(.c) void;
extern fn Luau_Ast_ParseResult_get_errors(*ParseResult) callconv(.c) zig_ParseResult_Errors;
extern fn Luau_Ast_ParseResult_free_errors(zig_ParseResult_Errors) callconv(.c) void;
extern fn Luau_Ast_ParseResult_hasNativeFunction(*ParseResult) callconv(.c) bool;

pub fn parse(source: []const u8, table: *NameTable, allocator: *Allocator) *ParseResult {
    return Luau_Ast_Parser_parse(
        source.ptr,
        source.len,
        table,
        allocator,
    );
}

pub const ParseResult = opaque {
    pub const HotComment = struct {
        header: bool,
        location: zig_Location,
        content: []const u8,
    };

    pub const HotComments = struct {
        allocator: std.mem.Allocator,
        values: []const HotComment,

        pub fn deinit(self: HotComments) void {
            for (self.values) |value|
                self.allocator.free(value.content);
            self.allocator.free(self.values);
        }
    };

    pub const ParseError = struct {
        location: zig_Location,
        message: []const u8,
    };

    pub const ParseErrors = struct {
        allocator: std.mem.Allocator,
        values: []const ParseError,

        pub fn deinit(self: ParseErrors) void {
            for (self.values) |value|
                self.allocator.free(value.message);
            self.allocator.free(self.values);
        }
    };

    pub inline fn deinit(self: *ParseResult) void {
        Luau_Ast_ParseResult_free(self);
    }

    pub fn getRoot(self: *ParseResult) *Node {
        return Luau_Ast_ParseResult_get_root(self);
    }

    pub fn getHotcomments(self: *ParseResult, allocator: std.mem.Allocator) !HotComments {
        const hotcomments = Luau_Ast_ParseResult_get_hotcomments(self);
        defer Luau_Ast_ParseResult_free_hotcomments(hotcomments);

        const arr = try allocator.alloc(HotComment, hotcomments.size);
        errdefer allocator.free(arr);
        errdefer for (arr) |comment| allocator.free(comment.content);

        for (0..hotcomments.size) |i| {
            const hotcomment = hotcomments.values[i];
            arr[i] = .{
                .header = hotcomment.header != 0,
                .location = hotcomment.location,
                .content = try allocator.dupe(u8, hotcomment.content[0..hotcomment.contentLen]),
            };
        }

        return .{
            .allocator = allocator,
            .values = arr,
        };
    }

    pub fn getErrors(self: *ParseResult, allocator: std.mem.Allocator) !ParseErrors {
        const errors = Luau_Ast_ParseResult_get_errors(self);
        defer Luau_Ast_ParseResult_free_errors(errors);

        const arr = try allocator.alloc(ParseError, errors.size);
        errdefer allocator.free(arr);
        errdefer for (arr) |comment| allocator.free(comment.message);

        for (0..errors.size) |i| {
            const err = errors.values[i];
            arr[i] = .{
                .location = err.location,
                .message = try allocator.dupe(u8, err.message[0..err.message_len]),
            };
        }

        return .{
            .allocator = allocator,
            .values = arr,
        };
    }

    pub fn hasNativeFunction(self: *ParseResult) bool {
        return Luau_Ast_ParseResult_hasNativeFunction(self);
    }
};

test ParseResult {
    var allocator = std.testing.allocator;

    {
        var a = Allocator.init(&allocator);
        defer a.deinit();

        var table = NameTable.init(a);
        defer table.deinit();
        const source =
            \\--!test
            \\-- This is a test comment
            \\local x = 
            \\
        ;

        var parse_result = parse(source, table, a);
        defer parse_result.deinit();

        try std.testing.expect(parse_result.hasNativeFunction() == false);
        {
            const hotcomments = try parse_result.getHotcomments(std.testing.allocator);
            defer hotcomments.deinit();

            try std.testing.expectEqual(1, hotcomments.values.len);
            const first = hotcomments.values[0];
            try std.testing.expectEqualStrings("test", first.content);
            try std.testing.expectEqual(true, first.header);
            try std.testing.expectEqual(0, first.location.begin.line);
            try std.testing.expectEqual(0, first.location.begin.column);
            try std.testing.expectEqual(0, first.location.end.line);
            try std.testing.expectEqual(7, first.location.end.column);
        }

        {
            const errors = try parse_result.getErrors(std.testing.allocator);
            defer errors.deinit();

            try std.testing.expectEqual(1, errors.values.len);
            const first = errors.values[0];
            try std.testing.expectEqualStrings("Expected identifier when parsing expression, got <eof>", first.message);
            try std.testing.expectEqual(3, first.location.begin.line);
            try std.testing.expectEqual(0, first.location.begin.column);
            try std.testing.expectEqual(3, first.location.end.line);
            try std.testing.expectEqual(0, first.location.end.column);
        }
    }
    {
        var a = Allocator.init(&allocator);
        defer a.deinit();

        var table = NameTable.init(a);
        defer table.deinit();
        const source =
            \\@native
            \\function test()
            \\end
            \\
        ;

        var parse_result = parse(source, table, a);
        defer parse_result.deinit();

        try std.testing.expect(parse_result.hasNativeFunction() == true);
    }
}

pub const Node = opaque {
    extern fn Luau_Ast_AstNode_get_class_index(self: *Node) callconv(.c) c_int;
    extern fn Luau_Ast_AstNode_get_location(self: *Node) callconv(.c) zig_Location;

    pub inline fn classIndex(self: *Node) c_int {
        return Luau_Ast_AstNode_get_class_index(self);
    }

    pub inline fn kind(self: *Node) NodeKind {
        return @enumFromInt(self.classIndex());
    }

    pub inline fn location(self: *Node) zig_Location {
        return Luau_Ast_AstNode_get_location(self);
    }

    pub fn cast(base: *Node, comptime to: NodeKind) ?*to.Type() {
        if (base.kind() == to) {
            // return @alignCast(@fieldParentPtr("base", base));
            return @ptrCast(base);
        }
        return null;
    }
};

/// AstName
pub const zig_AstName = [*:0]const u8;

pub const NodeKind = enum(c_int) {
    unknown,
    attr,
    generic_type,
    generic_type_pack,
    expr_group,
    expr_constant_nil,
    expr_constant_bool,
    expr_constant_number,
    expr_constant_string,
    expr_local,
    expr_global,
    expr_varargs,
    expr_call,
    expr_index_name,
    expr_index_expr,
    expr_function,
    expr_table,
    expr_unary,
    expr_binary,
    expr_type_assertion,
    expr_if_else,
    expr_interp_string,
    stat_block,
    stat_if,
    stat_while,
    stat_repeat,
    stat_break,
    stat_continue,
    stat_return,
    stat_expr,
    stat_local,
    stat_for,
    stat_for_in,
    stat_assign,
    stat_compound_assign,
    stat_function,
    stat_local_function,
    stat_type_alias,
    stat_type_function,
    stat_declare_global,
    stat_declare_function,
    stat_declare_class,
    type_reference,
    type_table,
    type_function,
    type_typeof,
    type_union,
    type_intersection,
    expr_error,
    stat_error,
    type_error,
    type_singleton_bool,
    type_singleton_string,
    type_group,
    type_pack_explicit,
    type_pack_variadic,
    type_pack_generic,

    pub fn Type(self: NodeKind) type {
        return switch (self) {
            .attr => Attr,
            .generic_type => GenericType,
            .generic_type_pack => GenericTypePack,
            .expr_group => ExprGroup,
            .expr_constant_nil => void,
            .expr_constant_bool => ExprConstantBool,
            .expr_constant_number => ExprConstantNumber,
            .expr_constant_string => ExprConstantString,
            .expr_local => ExprLocal,
            .expr_global => ExprGlobal,
            .expr_varargs => void,
            .expr_call => ExprCall,
            .expr_index_name => ExprIndexName,
            .expr_index_expr => ExprIndexExpr,
            .expr_function => ExprFunction,
            .expr_table => ExprTable,
            .stat_block => StatBlock,
            .stat_local => StatLocal,
            else => |kind| @compileError(@tagName(kind) ++ " not implemented"),
        };
    }
};

const TestParseContext = struct {
    allocator: *Allocator,
    table: *NameTable,

    pub fn init() TestParseContext {
        const a = Allocator.init(&std.testing.allocator);
        const table = NameTable.init(a);
        return .{ .allocator = a, .table = table };
    }

    pub fn deinit(self: TestParseContext) void {
        self.table.deinit();
        self.allocator.deinit();
    }

    pub fn parseContent(self: TestParseContext, source: []const u8) *ParseResult {
        return parse(source, self.table, self.allocator);
    }
};

pub const Attr = opaque {
    pub const Type = enum {
        checked,
        native,
    };

    extern fn Luau_Ast_Attr_get_type(*Attr) callconv(.c) c_int;

    pub inline fn getType(self: *Attr) Type {
        return @enumFromInt(Luau_Ast_Attr_get_type(self));
    }
};

test Attr {
    //     var context = TestParseContext.init();
    //     defer context.deinit();

    //     const source =
    //         \\@checked
    //         \\local x = 1
    //         \\
    //     ;
    //     const parsed = context.parseContent(source);
    //     defer parsed.deinit();

    //     const root = parsed.getRoot();
    //     const kind = root.kind();
    //     try std.testing.expectEqual(NodeKind.attr, kind);
    //     const as_attr = root.cast(.attr);
    //     try std.testing.expect(as_attr != null);
    //     try std.testing.expectEqual(Attr.Type.checked, as_attr.?.getType());
}

pub const TypeList = extern struct {
    types: [*]const *Node,
    types_len: usize,
    /// Null indicates no tail, not an untyped tail.
    tail_type: ?*Node,
};

/// used by GenericType and GenericTypePack
pub const NameNodeData = extern struct {
    name: [*:0]const u8,
    default_type: ?*Node = null,
};

pub const GenericType = opaque {
    extern fn Luau_Ast_GenericType_get(name: *GenericType) callconv(.c) NameNodeData;

    pub inline fn get(self: *GenericType) NameNodeData {
        return Luau_Ast_GenericType_get(self);
    }
};

pub const GenericTypePack = opaque {
    extern fn Luau_Ast_GenericTypePack_get(name: *GenericTypePack) callconv(.c) NameNodeData;

    pub inline fn get(self: *GenericTypePack) NameNodeData {
        return Luau_Ast_GenericTypePack_get(self);
    }
};

pub const ExprGroup = opaque {
    extern fn Luau_Ast_ExprGroup_get(*ExprGroup) callconv(.c) *Node;

    pub inline fn get(self: *ExprGroup) *Node {
        return Luau_Ast_ExprGroup_get(self);
    }
};

pub const ExprConstantBool = opaque {
    extern fn Luau_Ast_ExprConstantBool_get(*ExprConstantBool) callconv(.c) bool;

    pub inline fn get(self: *ExprConstantBool) bool {
        return Luau_Ast_ExprConstantBool_get(self);
    }
};

pub const ExprConstantNumber = opaque {
    pub const NumberParseResult = enum(u32) {
        ok,
        imprecise,
        malformed,
        bin_overflow,
        hex_overflow,
    };

    pub const Data = extern struct {
        value: f64,
        parse_result: NumberParseResult,
    };

    extern fn Luau_Ast_ExprConstantNumber_get(*ExprConstantNumber) callconv(.c) Data;

    pub inline fn get(self: *ExprConstantNumber) Data {
        return Luau_Ast_ExprConstantNumber_get(self);
    }
};

pub const ExprConstantString = opaque {
    pub const QuoteStyle = enum(u32) {
        simple,
        raw,
        unquoted,
    };

    pub const Data = extern struct {
        value: [*:0]const u8,
        len: usize,
        quote_style: QuoteStyle,

        pub fn string(self: Data) [:0]const u8 {
            return self.value[0..self.len];
        }
    };

    extern fn Luau_Ast_ExprConstantString_get(*ExprConstantString) callconv(.c) Data;

    pub inline fn get(self: *ExprConstantString) Data {
        return Luau_Ast_ExprConstantString_get(self);
    }
};

pub const Local = opaque {
    pub const Data = extern struct {
        name: [*:0]const u8,
        location: zig_Location,
        shadow: ?*Local,
        function_depth: usize,
        loop_depth: usize,
        annotation: ?*Node,
    };

    extern fn Luau_Ast_Local_get(*Local) callconv(.c) Data;

    pub inline fn get(self: *Local) Data {
        return Luau_Ast_Local_get(self);
    }
};

pub const ExprLocal = opaque {
    pub const Data = extern struct {
        local: *Local,
        upvalue: bool,
    };

    extern fn Luau_Ast_ExprLocal_get(*ExprLocal) callconv(.c) Data;

    pub inline fn get(self: *ExprLocal) Data {
        return Luau_Ast_ExprLocal_get(self);
    }
};

pub const ExprGlobal = opaque {
    extern fn Luau_Ast_ExprGlobal_get_name(*ExprGlobal) callconv(.c) [*:0]const u8;

    pub inline fn getName(self: *ExprGlobal) []const u8 {
        return std.mem.span(Luau_Ast_ExprGlobal_get_name(self));
    }
};

pub const ExprCall = opaque {
    pub const Data = extern struct {
        function: *Node,
        args: [*]const *Node,
        args_len: usize,
        self: bool,
        arg_location: zig_Location,
    };

    extern fn Luau_Ast_ExprCall_get(*ExprCall) callconv(.c) Data;

    pub inline fn get(self: *ExprCall) Data {
        return Luau_Ast_ExprCall_get(self);
    }
};

pub const ExprIndexName = opaque {
    pub const Data = extern struct {
        expr: *Node,
        index: [*:0]const u8,
        index_location: zig_Location,
        op_position: zig_Position,
        op: u8,
    };

    extern fn Luau_Ast_ExprIndexName_get(*ExprIndexName) callconv(.c) Data;

    pub inline fn get(self: *ExprIndexName) Data {
        return Luau_Ast_ExprIndexName_get(self);
    }
};

pub const ExprIndexExpr = opaque {
    pub const Data = extern struct {
        expr: *Node,
        index: *Node,
    };

    extern fn Luau_Ast_ExprIndexExpr_get(*ExprIndexExpr) callconv(.c) Data;

    pub inline fn get(self: *ExprIndexExpr) Data {
        return Luau_Ast_ExprIndexExpr_get(self);
    }
};

pub const ExprFunction = opaque {
    pub const Data = extern struct {
        attributes_: [*]Attr,
        attributes_len: usize,
        generics_: [*]GenericType,
        generics_len: usize,
        generic_packs_: [*]GenericTypePack,
        generic_packs_len: usize,
        self: *Local,
        args_: [*]Local,
        args_len: usize,
        has_return_annotation: bool,
        return_annotation: TypeList,
        vararg: bool,
        vararg_location: zig_Location,
        body: *StatBlock,
        function_depth: usize,
        debug_name: ?[*:0]const u8,
        has_arg_location: bool,
        arg_location: zig_Location,

        pub inline fn attributes(self: Data) []Attr {
            return self.attributes_[0..self.attributes_len];
        }

        pub inline fn generics(self: Data) []GenericType {
            return self.generics_[0..self.generics_len];
        }

        pub inline fn genericPacks(self: Data) []GenericTypePack {
            return self.generic_packs_[0..self.generic_packs_len];
        }

        pub inline fn args(self: Data) []Local {
            return self.args_[0..self.args_len];
        }
    };

    extern fn Luau_Ast_ExprFunction_get(*ExprFunction) callconv(.c) Data;

    pub inline fn get(self: *ExprFunction) Data {
        return Luau_Ast_ExprFunction_get(self);
    }
};

pub const ExprTable = opaque {
    pub const Item = extern struct {
        pub const Kind = enum(u32) { list, record, general };
        kind: Kind,
        key: ?*Node,
        value: *Node,
    };

    extern fn Luau_Ast_ExprTable_get_items(*ExprTable, len: *usize) callconv(.c) [*]const Item;

    pub inline fn getItems(self: *ExprTable) []const Item {
        var len: usize = 0;
        const items = Luau_Ast_ExprTable_get_items(self, &len);
        return items[0..len];
    }
};

pub const StatBlock = opaque {
    extern fn Luau_Ast_StatBlock_get_statements(*StatBlock, len: *usize) callconv(.c) [*]const *Node;

    pub inline fn getStatements(self: *StatBlock) []const *Node {
        var len: usize = 0;
        const statements = Luau_Ast_StatBlock_get_statements(self, &len);
        return statements[0..len];
    }
};

test StatBlock {
    {
        var context = TestParseContext.init();
        defer context.deinit();

        const source =
            \\local x: number = 1
            \\local y = 2
            \\local z = 3
            \\
        ;
        const parsed = context.parseContent(source);
        defer parsed.deinit();

        const root = parsed.getRoot();
        const kind = root.kind();
        try std.testing.expectEqual(NodeKind.stat_block, kind);
        const as_stat_block = root.cast(.stat_block);
        try std.testing.expect(as_stat_block != null);
        const statements = as_stat_block.?.getStatements();
        try std.testing.expectEqual(3, statements.len);

        for (statements) |statement| {
            try std.testing.expectEqual(NodeKind.stat_local, statement.kind());
            const as_stat_local: *StatLocal = statement.cast(.stat_local) orelse continue;
            std.debug.print("local {s} {?}\n", .{
                as_stat_local.get().vars()[0].get().name,
                as_stat_local.get().vars()[0].get().annotation,
            });
        }
    }
}

pub const StatLocal = opaque {
    pub const Data = extern struct {
        vars_: [*]*Local,
        vars_len: usize,
        values_: [*]*Node,
        values_len: usize,

        has_equals_sign_location: bool,
        equals_sign_location: zig_Location,

        pub inline fn vars(self: Data) []*Local {
            return self.vars_[0..self.vars_len];
        }

        pub inline fn values(self: Data) []const *Node {
            return self.values_[0..self.values_len];
        }
    };

    extern fn Luau_Ast_StatLocal_get(*StatLocal) callconv(.c) Data;

    pub inline fn get(self: *StatLocal) Data {
        return Luau_Ast_StatLocal_get(self);
    }
};

test Node {
    var allocator = std.testing.allocator;

    {
        var a = Allocator.init(&allocator);
        defer a.deinit();

        var table = NameTable.init(a);
        defer table.deinit();
        const source =
            \\local x = 1
            \\local y = 2
            \\local z = 3
            \\
        ;

        const parse_result = parse(source, table, a);
        defer parse_result.deinit();

        const root = parse_result.getRoot();

        const kind = root.kind();
        try std.testing.expectEqual(NodeKind.stat_block, kind);
    }
}
