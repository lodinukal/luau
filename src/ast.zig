const std = @import("std");

const Self = @This();

const luau = @import("root.zig");

extern "c" fn Luau_Ast_Lexer_AstNameTable_new(*luau.Allocator) *NameTable;
extern "c" fn Luau_Ast_Lexer_AstNameTable_free(*NameTable) void;

pub const NameTable = opaque {
    pub inline fn init(allocator: *luau.Allocator) *NameTable {
        return Luau_Ast_Lexer_AstNameTable_new(allocator);
    }

    pub inline fn deinit(self: *NameTable) void {
        Luau_Ast_Lexer_AstNameTable_free(self);
    }
};

test NameTable {
    var allocator = std.testing.allocator;
    var a = luau.Allocator.init(&allocator);
    defer a.deinit();

    var table = NameTable.init(a);
    defer table.deinit();
}

const Position = extern struct {
    line: c_uint,
    column: c_uint,
};

const Location = extern struct {
    begin: Position,
    end: Position,

    pub fn slice(self: Location, source: []const u8) ![]const u8 {
        var source_modified = source;
        var current_line: usize = 0;
        var current_column: usize = 0;

        var start: ?usize = null;
        for (source, 0..) |c, i| {
            if (current_line == self.begin.line and current_column == self.begin.column) {
                start = i;
            }
            if (current_line == self.end.line and current_column == self.end.column and start != null) {
                source_modified = source[start.?..i];
                return source_modified;
            }
            if (c == '\n') {
                current_line += 1;
                current_column = 0;
            } else {
                current_column += 1;
            }
        }
        return error.InvalidLocation;
    }
};

const zig_ParseResult_HotComment = extern struct {
    header: c_int,
    location: Location,
    content: [*]const u8,
    contentLen: usize,
};

const zig_ParseResult_HotComments = extern struct {
    values: [*]zig_ParseResult_HotComment,
    size: usize,
};

const Comment = extern struct {
    type: enum(u32) {
        comment = 282,
        block_comment = 283,
        broken_comment = 286,
        _,
    },
    location: Location,
};

const zig_ParseResult_Comments = extern struct {
    values: [*]Comment,
    size: usize,
};

const zig_ParseResult_Error = extern struct {
    location: Location,
    message: [*]const u8,
    message_len: usize,
};

const zig_ParseResult_Errors = extern struct {
    values: [*]zig_ParseResult_Error,
    size: usize,
};

extern fn Luau_Ast_Parser_parse([*]const u8, usize, *NameTable, *luau.Allocator) callconv(.c) *ParseResult;
extern fn Luau_Ast_ParseResult_free(*ParseResult) callconv(.c) void;
extern fn Luau_Ast_ParseResult_get_root(*ParseResult) callconv(.c) *Node;
extern fn Luau_Ast_ParseResult_get_cst_node(*ParseResult, *Node) callconv(.c) ?*CstNode;
extern fn Luau_Ast_ParseResult_get_comments(*ParseResult) callconv(.c) zig_ParseResult_Comments;
extern fn Luau_Ast_ParseResult_free_comments(zig_ParseResult_Comments) callconv(.c) void;
extern fn Luau_Ast_ParseResult_get_hotcomments(*ParseResult) callconv(.c) zig_ParseResult_HotComments;
extern fn Luau_Ast_ParseResult_free_hotcomments(zig_ParseResult_HotComments) callconv(.c) void;
extern fn Luau_Ast_ParseResult_get_errors(*ParseResult) callconv(.c) zig_ParseResult_Errors;
extern fn Luau_Ast_ParseResult_free_errors(zig_ParseResult_Errors) callconv(.c) void;
extern fn Luau_Ast_ParseResult_hasNativeFunction(*ParseResult) callconv(.c) bool;

pub fn parse(source: []const u8, table: *NameTable, allocator: *luau.Allocator) *ParseResult {
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
        location: Location,
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

    pub const Comments = struct {
        allocator: std.mem.Allocator,
        values: []const Comment,

        pub fn deinit(self: Comments) void {
            self.allocator.free(self.values);
        }
    };

    pub const ParseError = struct {
        location: Location,
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

    pub fn getCstNode(self: *ParseResult, node: *Node) ?*CstNode {
        return Luau_Ast_ParseResult_get_cst_node(self, node);
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

    pub fn getComments(self: *ParseResult, allocator: std.mem.Allocator) !Comments {
        const comments = Luau_Ast_ParseResult_get_comments(self);
        defer Luau_Ast_ParseResult_free_comments(comments);

        const arr = try allocator.dupe(Comment, comments.values[0..comments.size]);
        errdefer allocator.free(arr);

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
        var a = luau.Allocator.init(&allocator);
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

            const comments = try parse_result.getComments(std.testing.allocator);
            defer comments.deinit();

            try std.testing.expectEqual(2, comments.values.len);
            const first_normal = comments.values[1];
            const slice_of_source = try first_normal.location.slice(source);
            try std.testing.expectEqualStrings("-- This is a test comment", slice_of_source);
            try std.testing.expectEqual(1, first_normal.location.begin.line);
            try std.testing.expectEqual(0, first_normal.location.begin.column);
            try std.testing.expectEqual(1, first_normal.location.end.line);
            try std.testing.expectEqual(25, first_normal.location.end.column);
            try std.testing.expectEqual(.comment, first_normal.type);
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
        var a = luau.Allocator.init(&allocator);
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

pub const Name = extern struct {
    value: [*:0]const u8,
};

pub const Local = extern struct {
    name: Name,
    location: Location,
    shadow: ?*Local,
    functionDepth: usize,
    loopDepth: usize,
    annotation: ?*Node,
};

// matches layout of optional for msvc and clang
pub fn Optional(comptime T: type) type {
    return extern struct {
        value: T = undefined,
        hasValue: bool,

        pub fn to(self: @This()) ?T {
            if (self.hasValue) {
                return self.value;
            } else {
                return null;
            }
        }
    };
}

pub fn Array(comptime T: type) type {
    return extern struct {
        data: [*]T,
        size: usize,

        pub fn slice(self: @This()) []T {
            return self.data[0..self.size];
        }
    };
}

pub const TypeList = extern struct {
    types: Array(*Node),
    /// Null indicates no tail, not an untyped tail.
    tailType: ?*Node = null,
};

const CstNode = cst.Node;
const CstConstantNumber = cst.ExprConstantNumber;
const CstExprConstantString = cst.ExprConstantString;
const CstExprCall = cst.ExprCall;
const CstExprIndexExpr = cst.ExprIndexExpr;
const CstExprFunction = cst.ExprFunction;
const CstExprTable = cst.ExprTable;
const CstExprOp = cst.ExprOp;
const CstExprTypeAssertion = cst.ExprTypeAssertion;
const CstExprIfElse = cst.ExprIfElse;
const CstExprInterpString = cst.ExprInterpString;
const CstStatDo = cst.StatDo;
const CstStatRepeat = cst.StatRepeat;
const CstStatReturn = cst.StatReturn;
const CstStatLocal = cst.StatLocal;
const CstStatFor = cst.StatFor;
const CstStatForIn = cst.StatForIn;
const CstStatAssign = cst.StatAssign;
const CstStatCompoundAssign = cst.StatCompoundAssign;
const CstStatFunction = cst.StatFunction;
const CstStatLocalFunction = cst.StatLocalFunction;
const CstGenericType = cst.GenericType;
const CstGenericTypePack = cst.GenericTypePack;
const CstStatTypeAlias = cst.StatTypeAlias;
const CstStatTypeFunction = cst.StatTypeFunction;
const CstTypeReference = cst.TypeReference;
const CstTypeTable = cst.TypeTable;
const CstTypeFunction = cst.TypeFunction;
const CstTypeTypeof = cst.TypeTypeof;
const CstTypeUnion = cst.TypeUnion;
const CstTypeIntersection = cst.TypeIntersection;
const CstTypeSingletonString = cst.TypeSingletonString;
const CstTypePackExplicit = cst.TypePackExplicit;
const CstTypePackGeneric = cst.TypePackGeneric;

pub const cst = struct {
    pub const Node = extern struct {
        vtable: *const anyopaque,
        classIndex: Self.Node.Kind,

        pub const Kind = enum(u32) {
            expr_constant_number,
            expr_constant_string,
            expr_call,
            expr_index_expr,
            expr_function,
            expr_table,
            expr_op,
            expr_type_assertion,
            expr_if_else,
            expr_interp_string,
            stat_do,
            stat_repeat,
            stat_return,
            stat_local,
            stat_for,
            stat_for_in,
            stat_assign,
            stat_compound_assign,
            stat_function,
            stat_local_function,
            generic_type,
            generic_type_pack,
            stat_type_alias,
            stat_type_function,
            type_reference,
            type_table,
            type_function,
            type_typeof,
            type_union,
            type_intersection,
            type_singleton_string,
            type_pack_explicit,
            type_pack_generic,

            pub fn Type(self: Kind) type {
                return switch (self) {
                    .expr_constant_number => CstConstantNumber,
                    .expr_constant_string => CstExprConstantString,
                    .expr_call => CstExprCall,
                    .expr_index_expr => CstExprIndexExpr,
                    .expr_function => CstExprFunction,
                    .expr_table => CstExprTable,
                    .expr_op => CstExprOp,
                    .expr_type_assertion => CstExprTypeAssertion,
                    .expr_if_else => CstExprIfElse,
                    .expr_interp_string => CstExprInterpString,
                    .stat_do => CstStatDo,
                    .stat_repeat => CstStatRepeat,
                    .stat_return => CstStatReturn,
                    .stat_local => CstStatLocal,
                    .stat_for => CstStatFor,
                    .stat_for_in => CstStatForIn,
                    .stat_assign => CstStatAssign,
                    .stat_compound_assign => CstStatCompoundAssign,
                    .stat_function => CstStatFunction,
                    .stat_local_function => CstStatLocalFunction,
                    .generic_type => CstGenericType,
                    .generic_type_pack => CstGenericTypePack,
                    .stat_type_alias => CstStatTypeAlias,
                    .stat_type_function => CstStatTypeFunction,
                    .type_reference => CstTypeReference,
                    .type_table => CstTypeTable,
                    .type_function => CstTypeFunction,
                    .type_typeof => CstTypeTypeof,
                    .type_union => CstTypeUnion,
                    .type_intersection => CstTypeIntersection,
                    .type_singleton_string => CstTypeSingletonString,
                    .type_pack_explicit => CstTypePackExplicit,
                    .type_pack_generic => CstTypePackGeneric,

                    else => unreachable,
                };
            }
        };

        pub fn cast(base: *CstNode, comptime to: Kind) ?*to.Type() {
            if (base.classIndex == to) {
                // const InnerBase = @TypeOf(@field(@as(to.Type(), undefined), "base"));
                // const first_parent: *InnerBase = @alignCast(@fieldParentPtr("base", base));
                // return @alignCast(@fieldParentPtr("base", first_parent));
                return @ptrCast(base);
            }
            return null;
        }
    };

    pub const ExprConstantNumber = extern struct {
        vtable: *const anyopaque,
        classIndex: CstNode.Kind,

        value: Array(u8),
    };

    pub const ExprConstantString = extern struct {
        vtable: *const anyopaque,
        classIndex: CstNode.Kind,

        sourceString: Array(u8),
        quoteStyle: QuoteStyle,
        blockDepth: u32,

        pub const QuoteStyle = enum(u32) {
            quoted_single,
            quoted_double,
            quoted_raw,
            quoted_interp,
        };
    };

    pub const ExprCall = extern struct {
        vtable: *const anyopaque,
        classIndex: CstNode.Kind,

        openParens: Optional(Position),
        closeParens: Optional(Position),
        commaPositions: Array(Position),
    };

    pub const ExprIndexExpr = extern struct {
        vtable: *const anyopaque,
        classIndex: CstNode.Kind,

        openBracketPosition: Position,
        closeBracketPosition: Position,
    };

    pub const ExprFunction = extern struct {
        vtable: *const anyopaque,
        classIndex: CstNode.Kind,

        functionKeywordPosition: Position,
        openGenericsPosition: Position,
        genericsCommaPositions: Array(Position),
        closeGenericsPosition: Position,
        argsAnnotationColonPositions: Array(Position),
        argsCommaPositions: Array(Position),
        varargAnnotationColonPosition: Position,
        returnSpecifierPosition: Position,
    };

    pub const ExprTable = extern struct {
        vtable: *const anyopaque,
        classIndex: CstNode.Kind,

        items: Array(Item),

        pub const Item = struct {
            /// '[', only if Kind == General
            indexerOpenPosition: Optional(Position),
            /// ']', only if Kind == General
            indexerClosePosition: Optional(Position),
            /// only if Kind != List
            equalsPosition: Optional(Position),
            /// may be missing for last Item
            separator: Optional(Separator),
            /// may be missing for last Item
            separatorPosition: Optional(Position),

            pub const Separator = enum(u32) {
                comma,
                semicolon,
            };
        };
    };

    pub const ExprOp = extern struct {
        vtable: *const anyopaque,
        classIndex: CstNode.Kind,

        opPosition: Position,
    };

    pub const ExprTypeAssertion = extern struct {
        vtable: *const anyopaque,
        classIndex: CstNode.Kind,

        opPosition: Position,
    };

    pub const ExprIfElse = extern struct {
        vtable: *const anyopaque,
        classIndex: CstNode.Kind,

        thenPosition: Position,
        elsePosition: Position,
        isElseIf: bool,
    };

    pub const ExprInterpString = extern struct {
        vtable: *const anyopaque,
        classIndex: CstNode.Kind,

        sourceStrings: Array(Array(u8)),
        stringPositions: Array(Position),
    };

    pub const StatDo = extern struct {
        vtable: *const anyopaque,
        classIndex: CstNode.Kind,

        endPosition: Position,
    };

    pub const StatRepeat = extern struct {
        vtable: *const anyopaque,
        classIndex: CstNode.Kind,

        untilPosition: Position,
    };

    pub const StatReturn = extern struct {
        vtable: *const anyopaque,
        classIndex: CstNode.Kind,

        commaPositions: Array(Position),
    };

    pub const StatLocal = extern struct {
        vtable: *const anyopaque,
        classIndex: CstNode.Kind,

        varsAnnotationColonPositions: Array(Position),
        varsCommaPositions: Array(Position),
        valuesCommaPositions: Array(Position),
    };

    pub const StatFor = extern struct {
        vtable: *const anyopaque,
        classIndex: CstNode.Kind,

        annotationColonPosition: Position,
        equalsPosition: Position,
        endCommaPosition: Position,
        stepCommaPosition: Optional(Position),
    };

    pub const StatForIn = extern struct {
        vtable: *const anyopaque,
        classIndex: CstNode.Kind,

        varsAnnotationColonPositions: Array(Position),
        varsCommaPositions: Array(Position),
        valuesCommaPositions: Array(Position),
    };

    pub const StatAssign = extern struct {
        vtable: *const anyopaque,
        classIndex: CstNode.Kind,

        varsCommaPositions: Array(Position),
        equalsPosition: Position,
        valuesCommaPositions: Array(Position),
    };

    pub const StatCompoundAssign = extern struct {
        vtable: *const anyopaque,
        classIndex: CstNode.Kind,

        opPosition: Position,
    };

    pub const StatFunction = extern struct {
        vtable: *const anyopaque,
        classIndex: CstNode.Kind,

        functionKeywordPosition: Position,
    };

    pub const StatLocalFunction = extern struct {
        vtable: *const anyopaque,
        classIndex: CstNode.Kind,

        localKeywordPosition: Position,
        functionKeywordPosition: Position,
    };

    pub const GenericType = extern struct {
        vtable: *const anyopaque,
        classIndex: CstNode.Kind,

        defaultEqualsPosition: Optional(Position),
    };

    pub const GenericTypePack = extern struct {
        vtable: *const anyopaque,
        classIndex: CstNode.Kind,

        ellipsisPosition: Position,
        defaultEqualsPosition: Optional(Position),
    };

    pub const StatTypeAlias = extern struct {
        vtable: *const anyopaque,
        classIndex: CstNode.Kind,

        typeKeywordPosition: Position,
        genericsOpenPosition: Position,
        genericsCommaPositions: Array(Position),
        genericsClosePosition: Position,
        equalsPosition: Position,
    };

    pub const StatTypeFunction = extern struct {
        vtable: *const anyopaque,
        classIndex: CstNode.Kind,

        typeKeywordPosition: Position,
        functionKeywordPosition: Position,
    };

    pub const TypeReference = extern struct {
        vtable: *const anyopaque,
        classIndex: CstNode.Kind,

        prefixPointPosition: Optional(Position),
        openParametersPosition: Position,
        parametersCommaPositions: Array(Position),
        closeParametersPosition: Position,
    };

    pub const TypeTable = extern struct {
        vtable: *const anyopaque,
        classIndex: CstNode.Kind,

        items: Array(Item),
        isArray: bool,

        pub const Item = struct {
            kind: Kind,
            indexerOpenPosition: Optional(Position), // '[', only if Kind != Property
            indexerClosePosition: Optional(Position), // ']' only if Kind != Property
            colonPosition: Position,
            separator: Optional(CstExprTable.Item.Separator), // may be missing for last Item
            separatorPosition: Optional(Position),

            stringInfo: ?*CstExprConstantString, // only if Kind == StringProperty
            stringPosition: Position, // only if Kind == StringProperty

            pub const Kind = enum(u32) {
                indexer,
                property,
                string_property,
            };
        };
    };

    pub const TypeFunction = extern struct {
        vtable: *const anyopaque,
        classIndex: CstNode.Kind,

        openGenericsPosition: Position,
        genericsCommaPositions: Array(Position),
        closeGenericsPosition: Position,
        openArgsPosition: Position,
        argumentNameColonPositions: Array(Optional(Position)),
        argumentsCommaPositions: Array(Position),
        closeArgsPosition: Position,
        returnArrowPosition: Position,
    };

    pub const TypeTypeof = extern struct {
        vtable: *const anyopaque,
        classIndex: CstNode.Kind,

        openPosition: Position,
        closePosition: Position,
    };

    pub const TypeUnion = extern struct {
        vtable: *const anyopaque,
        classIndex: CstNode.Kind,

        leadingPosition: Optional(Position),
        separatorPositions: Array(Position),
    };

    pub const TypeIntersection = extern struct {
        vtable: *const anyopaque,
        classIndex: CstNode.Kind,

        leadingPosition: Optional(Position),
        separatorPositions: Array(Position),
    };

    pub const TypeSingletonString = extern struct {
        vtable: *const anyopaque,
        classIndex: CstNode.Kind,

        sourceString: Array(u8),
        quoteStyle: CstExprConstantString.QuoteStyle,
        blockDepth: u32,
    };

    pub const TypePackExplicit = extern struct {
        vtable: *const anyopaque,
        classIndex: CstNode.Kind,

        hasParentheses: bool,
        openParenthesesPosition: Position,
        closeParenthesesPosition: Position,
        commaPositions: Array(Position),
    };

    pub const TypePackGeneric = extern struct {
        vtable: *const anyopaque,
        classIndex: CstNode.Kind,

        ellipsisPosition: Position,
    };
};

pub const Node = extern struct {
    vtable: *const anyopaque,

    classIndex: Node.Kind,
    location: Location,

    pub const Kind = enum(u32) {
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
        stat_declare_extern_type,
        type_reference,
        type_table,
        type_function,
        type_typeof,
        type_optional,
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

        pub fn Type(self: Kind) type {
            return switch (self) {
                .unknown => Node,
                .attr => Attr,
                .generic_type => GenericType,
                .generic_type_pack => GenericTypePack,
                .expr_group => ExprGroup,
                .expr_constant_nil => ExprConstantNil,
                .expr_constant_bool => ExprConstantBool,
                .expr_constant_number => ExprConstantNumber,
                .expr_constant_string => ExprConstantString,
                .expr_local => ExprLocal,
                .expr_global => ExprGlobal,
                .expr_varargs => ExprVarargs,
                .expr_call => ExprCall,
                .expr_index_name => ExprIndexName,
                .expr_index_expr => ExprIndexExpr,
                .expr_function => ExprFunction,
                .expr_table => ExprTable,
                .expr_unary => ExprUnary,
                .expr_binary => ExprBinary,
                .expr_type_assertion => ExprTypeAssertion,
                .expr_if_else => ExprIfElse,
                .expr_interp_string => ExprInterpString,
                .stat_block => StatBlock,
                .stat_if => StatIf,
                .stat_while => StatWhile,
                .stat_repeat => StatRepeat,
                .stat_break => StatBreak,
                .stat_continue => StatContinue,
                .stat_return => StatReturn,
                .stat_expr => StatExpr,
                .stat_local => StatLocal,
                .stat_for => StatFor,
                .stat_for_in => StatForIn,
                .stat_assign => StatAssign,
                .stat_compound_assign => StatCompoundAssign,
                .stat_function => StatFunction,
                .stat_local_function => StatLocalFunction,
                .stat_type_alias => StatTypeAlias,
                .stat_type_function => StatTypeFunction,
                .stat_declare_global => StatDeclareGlobal,
                .stat_declare_function => StatDeclareFunction,
                .stat_declare_extern_type => StatDeclareExternType,
                .type_reference => TypeReference,
                .type_table => TypeTable,
                .type_function => TypeFunction,
                .type_typeof => TypeTypeof,
                .type_optional => TypeOptional,
                .type_union => TypeUnion,
                .type_intersection => TypeIntersection,
                .expr_error => ExprError,
                .stat_error => StatError,
                .type_error => TypeError,
                .type_singleton_bool => TypeSingletonBool,
                .type_singleton_string => TypeSingletonString,
                .type_group => TypeGroup,
                .type_pack_explicit => TypePackExplicit,
                .type_pack_variadic => TypePackVariadic,
                .type_pack_generic => TypePackGeneric,
            };
        }
    };

    pub fn cast(base: *Node, comptime to: Kind) ?*to.Type() {
        if (base.classIndex == to) {
            // const InnerBase = @TypeOf(@field(@as(to.Type(), undefined), "base"));
            // const first_parent: *InnerBase = @alignCast(@fieldParentPtr("base", base));
            // return @alignCast(@fieldParentPtr("base", first_parent));
            return @ptrCast(base);
        }
        return null;
    }
};

pub const Attr = extern struct {
    vtable: *const anyopaque,

    classIndex: Node.Kind,
    location: Location,

    kind: Node,

    pub const Kind = enum(c_int) {
        checked = 0,
        native = 1,
        deprecated = 2,
    };
};

pub const GenericType = extern struct {
    vtable: *const anyopaque,

    classIndex: Node.Kind,
    location: Location,

    name: Name,
    defaultValue: ?*Node = null,
};

pub const GenericTypePack = extern struct {
    vtable: *const anyopaque,

    classIndex: Node.Kind,
    location: Location,

    name: Name,
    defaultValue: ?*Node = null,
};

pub const ExprGroup = extern struct {
    vtable: *const anyopaque,

    classIndex: Node.Kind,
    location: Location,

    expr: ?*Node,
};

pub const ExprConstantNil = extern struct {
    vtable: *const anyopaque,

    classIndex: Node.Kind,
    location: Location,
};

pub const ExprConstantBool = extern struct {
    vtable: *const anyopaque,

    classIndex: Node.Kind,
    location: Location,

    value: bool,
};

pub const ConstantNumberParseResult = enum(c_int) {
    Ok = 0,
    Imprecise = 1,
    Malformed = 2,
    BinOverflow = 3,
    HexOverflow = 4,
};

pub const ExprConstantNumber = extern struct {
    vtable: *const anyopaque,

    classIndex: Node.Kind,
    location: Location,

    value: f64,
    parseResult: ConstantNumberParseResult,
};

pub const ExprConstantString = extern struct {
    vtable: *const anyopaque,

    classIndex: Node.Kind,
    location: Location,

    value: Array(u8),
    quoteStyle: QuoteStyle,

    pub const QuoteStyle = enum(c_int) {
        QuotedSimple = 0,
        QuotedRaw = 1,
        Unquoted = 2,
    };
};

pub const ExprLocal = extern struct {
    vtable: *const anyopaque,

    classIndex: Node.Kind,
    location: Location,

    local: *Local,
    upvalue: bool,
};

pub const ExprGlobal = extern struct {
    vtable: *const anyopaque,

    classIndex: Node.Kind,
    location: Location,

    name: Name,
};

pub const ExprVarargs = extern struct {
    vtable: *const anyopaque,

    classIndex: Node.Kind,
    location: Location,
};

pub const ExprCall = extern struct {
    vtable: *const anyopaque,

    classIndex: Node.Kind,
    location: Location,

    func: *Node,
    args: Array(*Node),
    self: bool,
    argLocation: Location,
};

pub const ExprIndexName = extern struct {
    vtable: *const anyopaque,

    classIndex: Node.Kind,
    location: Location,

    expr: *Node,
    index: Name,
    indexLocation: Location,
    opPosition: Position,
    op: u8,
};

pub const ExprIndexExpr = extern struct {
    vtable: *const anyopaque,

    classIndex: Node.Kind,
    location: Location,

    expr: *Node,
    index: *Node,
};

pub const ExprFunction = extern struct {
    vtable: *const anyopaque,

    classIndex: Node.Kind,
    location: Location,

    attributes: Array(*Attr),
    generics: Array(*GenericType),
    genericPacks: Array(*GenericTypePack),
    self: *Local,
    args: Array(*Local),
    returnAnnotation_DEPRECATED: Optional(TypeList),
    /// *AstTypePack
    returnAnnotation: ?*Node,
    vararg: bool = false,
    varargLocation: Location,
    varargAnnotation: *Node,
    body: *StatBlock,
    functionDepth: usize,
    debugname: Name,
    argLocation: Optional(Location),
};

pub const ExprTable = extern struct {
    vtable: *const anyopaque,

    classIndex: Node.Kind,
    location: Location,

    items: Array(Item),

    pub const Item = extern struct {
        pub const Kind = enum(u32) {
            /// foo
            List = 0,
            /// foo=bar
            Record = 1,
            /// [foo]=bar
            General = 2,
        };

        kind: Kind,
        /// can be nullptr!
        key: ?*Node,
        value: *Node,
    };
};

pub const ExprUnary = extern struct {
    vtable: *const anyopaque,

    classIndex: Node.Kind,
    location: Location,

    op: Op,
    expr: *Node,

    pub const Op = enum(c_int) {
        Not = 0,
        Minus = 1,
        Len = 2,
    };
};

pub const ExprBinary = extern struct {
    vtable: *const anyopaque,

    classIndex: Node.Kind,
    location: Location,

    op: Op,
    left: *Node,
    right: *Node,

    pub const Op = enum(c_int) {
        Add = 0,
        Sub = 1,
        Mul = 2,
        Div = 3,
        FloorDiv = 4,
        Mod = 5,
        Pow = 6,
        Concat = 7,
        CompareNe = 8,
        CompareEq = 9,
        CompareLt = 10,
        CompareLe = 11,
        CompareGt = 12,
        CompareGe = 13,
        And = 14,
        Or = 15,
    };
};

pub const ExprTypeAssertion = extern struct {
    vtable: *const anyopaque,

    classIndex: Node.Kind,
    location: Location,

    expr: *Node,
    annotation: *Node,
};

pub const ExprIfElse = extern struct {
    vtable: *const anyopaque,

    classIndex: Node.Kind,
    location: Location,

    condition: *Node,
    hasThen: bool,
    trueExpr: ?*Node,
    hasElse: bool,
    falseExpr: ?*Node,
};

pub const ExprInterpString = extern struct {
    vtable: *const anyopaque,

    classIndex: Node.Kind,
    location: Location,

    /// An interpolated string such as `foo{bar}baz` is represented as
    /// an array of strings for "foo" and "bar", and an array of expressions for "baz".
    /// `strings` will always have one more element than `expressions`.
    strings: Array(Array(u8)),
    expressions: Array(*Node),
};

pub const StatBlock = extern struct {
    vtable: *const anyopaque,

    classIndex: Node.Kind,
    location: Location,
    MAYBE_hasSemicolon: bool = false,

    body: Array(*Node),
    /// Indicates whether or not this block has been terminated in a
    /// syntactically valid way.
    ///
    /// This is usually but not always done with the 'end' keyword.  StatIf
    /// and StatRepeat are the two main exceptions to this.
    ///
    /// The 'then' clause of an if statement can properly be closed by the
    /// keywords 'else' or 'elseif'.  A 'repeat' loop's body is closed with the
    /// 'until' keyword.
    hasEnd: bool = false,
};

pub const StatIf = extern struct {
    vtable: *const anyopaque,

    classIndex: Node.Kind,
    location: Location,
    MAYBE_hasSemicolon: bool = false,

    condition: *Node,
    thenbody: *StatBlock,
    elsebody: ?*Node,
    thenLocation: Optional(Location),
    /// Active for 'elseif' as well
    elseLocation: Optional(Location),
};

pub const StatWhile = extern struct {
    vtable: *const anyopaque,

    classIndex: Node.Kind,
    location: Location,
    MAYBE_hasSemicolon: bool = false,

    condition: *Node,
    body: *StatBlock,
    hasDo: bool = false,
    doLocation: Location,
};

pub const StatRepeat = extern struct {
    vtable: *const anyopaque,

    classIndex: Node.Kind,
    location: Location,
    MAYBE_hasSemicolon: bool = false,

    condition: *Node,
    body: *StatBlock,
    DEPRECATED_hasUntil: bool = false,
};

pub const StatBreak = extern struct {
    vtable: *const anyopaque,

    classIndex: Node.Kind,
    location: Location,
    MAYBE_hasSemicolon: bool = false,
};

pub const StatContinue = extern struct {
    vtable: *const anyopaque,

    classIndex: Node.Kind,
    location: Location,
    MAYBE_hasSemicolon: bool = false,
};

pub const StatReturn = extern struct {
    vtable: *const anyopaque,

    classIndex: Node.Kind,
    location: Location,
    MAYBE_hasSemicolon: bool = false,

    list: Array(*Node),
};

pub const StatExpr = extern struct {
    vtable: *const anyopaque,

    classIndex: Node.Kind,
    location: Location,
    MAYBE_hasSemicolon: bool = false,

    expr: *Node,
};

pub const StatLocal = extern struct {
    vtable: *const anyopaque,

    classIndex: Node.Kind,
    location: Location,
    MAYBE_hasSemicolon: bool = false,

    vars: Array(*Local),
    values: Array(*Node),
    equalsSignLocation: Optional(Location),
};

pub const StatFor = extern struct {
    vtable: *const anyopaque,

    classIndex: Node.Kind,
    location: Location,
    MAYBE_hasSemicolon: bool = false,

    variable: *Local,
    from: *Node,
    to: *Node,
    step: ?*Node,
    body: *StatBlock,
    hasDo: bool = false,
    doLocation: Location,
};

pub const StatForIn = extern struct {
    vtable: *const anyopaque,

    classIndex: Node.Kind,
    location: Location,
    MAYBE_hasSemicolon: bool = false,

    vars: Array(*Local),
    values: Array(*Node),
    body: *StatBlock,
    hasIn: bool = false,
    inLocation: Location,
    hasDo: bool = false,
    doLocation: Location,
};

pub const StatAssign = extern struct {
    vtable: *const anyopaque,

    classIndex: Node.Kind,
    location: Location,
    MAYBE_hasSemicolon: bool = false,

    vars: Array(*Node),
    values: Array(*Node),
};

pub const StatCompoundAssign = extern struct {
    vtable: *const anyopaque,

    classIndex: Node.Kind,
    location: Location,
    MAYBE_hasSemicolon: bool = false,

    op: ExprBinary.Op,
    variable: *Node,
    value: *Node,
};

pub const StatFunction = extern struct {
    vtable: *const anyopaque,

    classIndex: Node.Kind,
    location: Location,
    MAYBE_hasSemicolon: bool = false,

    name: *Node,
    func: *ExprFunction,
};

pub const StatLocalFunction = extern struct {
    vtable: *const anyopaque,

    classIndex: Node.Kind,
    location: Location,
    MAYBE_hasSemicolon: bool = false,

    name: *Local,
    func: *ExprFunction,
};

pub const StatTypeAlias = extern struct {
    vtable: *const anyopaque,

    classIndex: Node.Kind,
    location: Location,
    MAYBE_hasSemicolon: bool = false,

    name: Name,
    nameLocation: Location,
    generics: Array(*GenericType),
    genericPacks: Array(*GenericTypePack),
    type: *Node,
    exported: bool,
};

pub const StatTypeFunction = extern struct {
    vtable: *const anyopaque,

    classIndex: Node.Kind,
    location: Location,
    MAYBE_hasSemicolon: bool = false,

    name: Name,
    nameLocation: Location,
    body: *ExprFunction,
    exported: bool,
    hasErrors: bool,
};

pub const StatDeclareGlobal = extern struct {
    vtable: *const anyopaque,

    classIndex: Node.Kind,
    location: Location,
    MAYBE_hasSemicolon: bool = false,

    name: Name,
    nameLocation: Location,
    type: *Node,
};

pub const ArgumentName = extern struct {
    name: Name,
    location: Location,
};

pub const StatDeclareFunction = extern struct {
    vtable: *const anyopaque,

    classIndex: Node.Kind,
    location: Location,
    MAYBE_hasSemicolon: bool = false,

    attributes: Array(*Attr),
    name: Name,
    nameLocation: Location,
    generics: Array(*GenericType),
    genericPacks: Array(*GenericTypePack),
    params: TypeList,
    paramNames: Array(ArgumentName),
    vararg: bool = false,
    varargLocation: Location,
    /// *AstTypePack
    retTypes: *Node,
    retTypes_DEPRECATED: TypeList,
};

pub const DeclaredExternTypeProperty = extern struct {
    name: Name,
    nameLocation: Location,
    ty: ?*Node = null,
    isMethod: bool = false,
    location: Location,
};

pub const TableAccess = enum(u32) {
    Read = 1,
    Write = 2,
    ReadWrite = 3,
};

pub const TableIndexer = extern struct {
    indexType: *Node,
    resultType: *Node,
    location: Location,
    access: TableAccess = .ReadWrite,
    accessLocation: Optional(Location),
};

pub const StatDeclareExternType = extern struct {
    vtable: *const anyopaque,

    classIndex: Node.Kind,
    location: Location,
    MAYBE_hasSemicolon: bool = false,

    name: Name,
    superName: Optional(Name),
    props: Array(DeclaredExternTypeProperty),
    indexer: ?*TableIndexer,
};

/// Don't have Luau::Variant available, it's a bit of an overhead, but a plain struct is nice to use
pub const TypeOrPack = extern struct {
    type: ?*Node = null,
    typePack: ?*Node = null,
};

pub const TypeReference = extern struct {
    vtable: *const anyopaque,

    classIndex: Node.Kind,
    location: Location,

    hasParameterList: bool,
    prefix: Optional(Name),
    prefixLocation: Optional(Location),
    name: Name,
    nameLocation: Location,
    parameters: Array(TypeOrPack),
};

pub const TableProp = extern struct {
    name: Name,
    location: Location,
    type: *Node,
    access: TableAccess = .ReadWrite,
    accessLocation: Optional(Location),
};

pub const TypeTable = extern struct {
    vtable: *const anyopaque,

    classIndex: Node.Kind,
    location: Location,

    props: Array(TableProp),
    indexer: ?*TableIndexer,
};

pub const TypeFunction = extern struct {
    vtable: *const anyopaque,

    classIndex: Node.Kind,
    location: Location,

    attributes: Array(*Attr),
    generics: Array(*GenericType),
    genericPacks: Array(*GenericTypePack),
    argTypes: TypeList,
    argNames: Array(Optional(ArgumentName)),
    returnTypes_DEPRECATED: TypeList,
    /// *AstTypePack
    returnTypes: *Node,
};

pub const TypeTypeof = extern struct {
    vtable: *const anyopaque,

    classIndex: Node.Kind,
    location: Location,

    expr: *Node,
};

pub const TypeOptional = extern struct {
    vtable: *const anyopaque,

    classIndex: Node.Kind,
    location: Location,

    type: *Node,
};

pub const TypeUnion = extern struct {
    vtable: *const anyopaque,

    classIndex: Node.Kind,
    location: Location,

    types: Array(*Node),
};

pub const TypeIntersection = extern struct {
    vtable: *const anyopaque,

    classIndex: Node.Kind,
    location: Location,

    types: Array(*Node),
};

pub const ExprError = extern struct {
    vtable: *const anyopaque,

    classIndex: Node.Kind,
    location: Location,

    expressions: Array(*Node),
    messageIndex: c_uint,
};

pub const StatError = extern struct {
    vtable: *const anyopaque,

    classIndex: Node.Kind,
    location: Location,
    MAYBE_hasSemicolon: bool = false,

    expressions: Array(*Node),
    statements: Array(*Node),
    messageIndex: c_uint,
};

pub const TypeError = extern struct {
    vtable: *const anyopaque,

    classIndex: Node.Kind,
    location: Location,

    types: Array(*Node),
    isMissing: bool,
    messageIndex: c_uint,
};

pub const TypeSingletonBool = extern struct {
    vtable: *const anyopaque,

    classIndex: Node.Kind,
    location: Location,

    value: bool,
};

pub const TypeSingletonString = extern struct {
    vtable: *const anyopaque,

    classIndex: Node.Kind,
    location: Location,

    value: Array(u8),
};

pub const TypeGroup = extern struct {
    vtable: *const anyopaque,

    classIndex: Node.Kind,
    location: Location,

    type: *Node,
};

pub const TypePackExplicit = extern struct {
    vtable: *const anyopaque,

    classIndex: Node.Kind,
    location: Location,

    typeList: TypeList,
};

pub const TypePackVariadic = extern struct {
    vtable: *const anyopaque,

    classIndex: Node.Kind,
    location: Location,

    variadicType: *Node,
};

pub const TypePackGeneric = extern struct {
    vtable: *const anyopaque,

    classIndex: Node.Kind,
    location: Location,

    genericName: Name,
};

test Node {
    var allocator = std.testing.allocator;

    {
        var a = luau.Allocator.init(&allocator);
        defer a.deinit();

        var table = NameTable.init(a);
        defer table.deinit();
        const source =
            \\local x = 1;
            \\local x = 2
            \\local x = 3
            \\
        ;

        const parse_result = parse(source, table, a);
        defer parse_result.deinit();

        const root = parse_result.getRoot();

        try std.testing.expectEqual(Node.Kind.stat_block, root.classIndex);
        const block: *StatBlock = root.cast(.stat_block).?;

        const stats = block.body.slice();
        try std.testing.expectEqual(3, stats.len);

        for (stats) |node| {
            switch (node.classIndex) {
                .stat_local => {
                    const local: *StatLocal = node.cast(.stat_local).?;
                    try std.testing.expectEqualStrings("x", std.mem.span(local.vars.slice()[0].name.value));
                    // std.log.err("{}", .{(local.values.slice()[0].cast(.expr_constant_number).?.value)});
                },
                else => {},
            }
        }
    }
}

comptime {
    std.testing.refAllDecls(@This());
}
