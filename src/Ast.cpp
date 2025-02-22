#include "common.h"

#include "Luau/Allocator.h"
#include "Luau/Ast.h"
#include "Luau/Lexer.h"
#include "Luau/Parser.h"

#define ZIG_LUAU_AST(name) (Luau_Ast_##name)

EXPORT Luau::Allocator* ZIG_LUAU_AST(Allocator_new)(void* ctx, Luau::AllocatorVTable vtable)
{
    auto* returned = new Luau::Allocator(ctx, vtable);
    return returned;
}

EXPORT Luau::Allocator* ZIG_LUAU_AST(Allocator_default)()
{
    auto* returned = new Luau::Allocator();
    return returned;
}

EXPORT void ZIG_LUAU_AST(Allocator_free)(Luau::Allocator* value)
{
    delete value;
}

EXPORT Luau::AstNameTable* ZIG_LUAU_AST(Lexer_AstNameTable_new)(Luau::Allocator* allocator)
{
    return new Luau::AstNameTable(*allocator);
}

EXPORT void ZIG_LUAU_AST(Lexer_AstNameTable_free)(Luau::AstNameTable* value)
{
    delete value;
}

EXPORT Luau::ParseResult* ZIG_LUAU_AST(Parser_parse)(const char* source, size_t sourceLen, Luau::AstNameTable* names, Luau::Allocator* allocator)
{
    Luau::ParseOptions parseOptions;
    Luau::ParseResult result = Luau::Parser::parse(source, sourceLen, *names, *allocator, parseOptions);
    return new Luau::ParseResult(result);
}

EXPORT void ZIG_LUAU_AST(ParseResult_free)(Luau::ParseResult* value)
{
    delete value;
}

EXPORT Luau::AstNode* ZIG_LUAU_AST(ParseResult_get_root)(Luau::ParseResult* value)
{
    return value->root;
}

EXPORT struct zig_Position
{
    unsigned int line;
    unsigned int column;
};

EXPORT struct zig_Location
{
    zig_Position begin;
    zig_Position end;
};

EXPORT struct zig_ParseResult_HotComment
{
    bool header;
    zig_Location location;
    const char* content;
    size_t contentLen;
};

EXPORT struct zig_ParseResult_HotComments
{
    zig_ParseResult_HotComment* values;
    size_t size;
};

EXPORT struct zig_ParseResult_Error
{
    zig_Location location;
    const char* message;
    size_t messageLen;
};

EXPORT struct zig_ParseResult_Errors
{
    zig_ParseResult_Error* values;
    size_t size;
};

EXPORT zig_ParseResult_HotComments ZIG_LUAU_AST(ParseResult_get_hotcomments)(Luau::ParseResult* value)
{
    size_t size = value->hotcomments.size();
    zig_ParseResult_HotComment* values = new zig_ParseResult_HotComment[size];
    for (size_t i = 0; i < size; i++)
    {
        Luau::HotComment hotcomment = value->hotcomments[i];

        size_t contentLen = hotcomment.content.size();

        char* buffer = new char[contentLen];
        memcpy(buffer, hotcomment.content.c_str(), contentLen);

        values[i] = {
            hotcomment.header,
            {
                {hotcomment.location.begin.line, hotcomment.location.begin.column},
                {hotcomment.location.end.line, hotcomment.location.end.column},
            },
            buffer,
            contentLen
        };
    }
    return {values, size};
}

EXPORT void ZIG_LUAU_AST(ParseResult_free_hotcomments)(zig_ParseResult_HotComments comments)
{
    for (size_t i = 0; i < comments.size; i++)
    {
        zig_ParseResult_HotComment hotcomment = comments.values[i];
        delete[] hotcomment.content;
    }
    delete[] comments.values;
}

EXPORT zig_ParseResult_Errors ZIG_LUAU_AST(ParseResult_get_errors)(Luau::ParseResult* value)
{
    size_t size = value->errors.size();
    zig_ParseResult_Error* values = new zig_ParseResult_Error[size];
    for (size_t i = 0; i < size; i++)
    {
        Luau::ParseError error = value->errors[i];

        const std::string message = error.getMessage();
        const Luau::Location location = error.getLocation();

        size_t messageLen = message.size();

        char* buffer = new char[messageLen];
        memcpy(buffer, message.c_str(), messageLen);

        values[i] = {
            {
                {location.begin.line, location.begin.column},
                {location.end.line, location.end.column},
            },
            buffer,
            messageLen
        };
    }
    return {values, size};
}

EXPORT void ZIG_LUAU_AST(ParseResult_free_errors)(zig_ParseResult_Errors errors)
{
    for (size_t i = 0; i < errors.size; i++)
    {
        zig_ParseResult_Error error = errors.values[i];
        delete[] error.message;
    }
    delete[] errors.values;
}

// Code based on https://github.com/luau-lang/luau/blob/8cc289fae430d9b0c22bde209fe5cf2d01751ff1/Compiler/src/Compiler.cpp#L3906
struct FunctionVisitor : Luau::AstVisitor
{
    std::vector<Luau::AstExprFunction*>& functions;
    bool hasTypes = false;
    bool hasNativeFunction = false;

    FunctionVisitor(std::vector<Luau::AstExprFunction*>& functions)
        : functions(functions)
    {
        // preallocate the result; this works around std::vector's inefficient growth policy for small arrays
        functions.reserve(16);
    }

    bool visit(Luau::AstExprFunction* node) override
    {
        node->body->visit(this);

        for (Luau::AstLocal* arg : node->args)
        {
            hasTypes |= arg->annotation != nullptr;
        }

        // this makes sure all functions that are used when compiling this one have been already added to the vector
        functions.push_back(node);

        if (!hasNativeFunction && node->hasNativeAttribute())
        {
            hasNativeFunction = true;
        }

        return false;
    }
};

EXPORT bool ZIG_LUAU_AST(ParseResult_hasNativeFunction)(Luau::ParseResult* value)
{
    std::vector<Luau::AstExprFunction*> functions;
    FunctionVisitor functionVisitor(functions);
    value->root->visit(&functionVisitor);
    return functionVisitor.hasNativeFunction;
}

EXPORT int ZIG_LUAU_AST(AstNode_get_class_index)(Luau::AstNode* value)
{
    return value->classIndex;
}

EXPORT zig_Location ZIG_LUAU_AST(AstNode_get_location)(Luau::AstNode* value)
{
    const Luau::Location location = value->location;
    return {
        {location.begin.line, location.begin.column},
        {location.end.line, location.end.column},
    };
}

// TypeList
EXPORT struct zig_TypeList
{
    Luau::AstType** types;
    size_t types_len;
    Luau::AstTypePack* tailType;
};

// Attr
EXPORT Luau::AstAttr::Type ZIG_LUAU_AST(Attr_get_type)(Luau::AstAttr* value)
{
    return value->type;
}

// GenericType
EXPORT struct zig_NameNode
{
    const char* name;
    Luau::AstNode* type;
};

EXPORT zig_NameNode ZIG_LUAU_AST(GenericType_get)(Luau::AstGenericType* value)
{
    return {value->name.value, value->defaultValue};
}

// GenericTypePack
EXPORT zig_NameNode ZIG_LUAU_AST(GenericTypePackExplicit_get)(Luau::AstGenericTypePack* value)
{
    return {value->name.value, value->defaultValue};
}

// ExprGroup
EXPORT Luau::AstExpr* ZIG_LUAU_AST(ExprGroup_get_expr)(Luau::AstExprGroup* value)
{
    return value->expr;
}

// ExprConstantBool
EXPORT bool ZIG_LUAU_AST(ExprConstantBool_get)(Luau::AstExprConstantBool* value)
{
    return value->value;
}

// ExprConstantNumber
EXPORT struct zig_ExprConstantNumber
{
    double value;
    Luau::ConstantNumberParseResult parseResult;
};

EXPORT zig_ExprConstantNumber ZIG_LUAU_AST(ExprConstantNumber_get)(Luau::AstExprConstantNumber* value)
{
    return {value->value, value->parseResult};
}

// ExprConstantString
EXPORT struct zig_ExprConstantString
{
    const char* value;
    size_t valueLen;
    Luau::AstExprConstantString::QuoteStyle quoteStyle;
};

EXPORT zig_ExprConstantString ZIG_LUAU_AST(ExprConstantString_get)(Luau::AstExprConstantString* value)
{
    return {value->value.data, value->value.size, value->quoteStyle};
}

// Local
EXPORT struct zig_Local
{
    const char* name;
    zig_Location location;
    Luau::AstLocal* shadow;
    size_t functionDepth;
    size_t loopDepth;
    Luau::AstNode* annotation;
};

EXPORT zig_Local ZIG_LUAU_AST(Local_get)(Luau::AstLocal* value)
{
    return {
        value->name.value,
        {
            {value->location.begin.line, value->location.begin.column},
            {value->location.end.line, value->location.end.column},
        },
        value->shadow,
        value->functionDepth,
        value->loopDepth,
        value->annotation
    };
}

// ExprLocal
EXPORT struct zig_ExprLocal
{
    Luau::AstLocal* local;
    bool upvalue;
};

EXPORT zig_ExprLocal ZIG_LUAU_AST(ExprLocal_get)(Luau::AstExprLocal* value)
{
    return {value->local, value->upvalue};
}

// ExprGlobal
EXPORT const char* ZIG_LUAU_AST(ExprGlobal_get_name)(Luau::AstExprGlobal* value)
{
    return value->name.value;
}

// ExprCall
EXPORT struct zig_ExprCall
{
    Luau::AstExpr* function;
    Luau::AstExpr** args;
    size_t args_len;
    bool self;
    zig_Location argLocation;
};

EXPORT zig_ExprCall ZIG_LUAU_AST(ExprCall_get)(Luau::AstExprCall* value)
{
    return {
        value->func,
        value->args.data,
        value->args.size,
        value->self,
        {
            {value->argLocation.begin.line, value->argLocation.begin.column},
            {value->argLocation.end.line, value->argLocation.end.column},
        }
    };
}

// ExprIndexName
EXPORT struct zig_ExprIndexName
{
    Luau::AstExpr* expr;
    const char* index;
    zig_Location index_location;
    zig_Position op_position;
    char op;
};

EXPORT zig_ExprIndexName ZIG_LUAU_AST(ExprIndexName_get)(Luau::AstExprIndexName* value)
{
    return {
        value->expr,
        value->index.value,
        {
            {value->indexLocation.begin.line, value->indexLocation.begin.column},
            {value->indexLocation.end.line, value->indexLocation.end.column},
        },
        {value->opPosition.line, value->opPosition.column},
        value->op
    };
}

// ExprIndexExpr
EXPORT struct zig_ExprIndexExpr
{
    Luau::AstExpr* expr;
    Luau::AstExpr* index;
};

EXPORT zig_ExprIndexExpr ZIG_LUAU_AST(ExprIndexExpr_get)(Luau::AstExprIndexExpr* value)
{
    return {value->expr, value->index};
}

// ExprFunction
EXPORT struct zig_ExprFunction
{
    Luau::AstAttr** attributes;
    size_t attributes_len;
    Luau::AstGenericType** generics;
    size_t generics_len;
    Luau::AstGenericTypePack** genericPacks;
    size_t genericPacks_len;
    Luau::AstLocal* self;
    Luau::AstLocal** args;
    size_t args_len;
    bool hasReturnAnnotation;
    zig_TypeList returnAnnotation;
    bool vararg;
    zig_Location varargLocation;
    Luau::AstStatBlock* body;
    size_t functionDepth;
    const char* debugName;
    bool hasArgLocation;
    zig_Location argLocation;
};

EXPORT zig_ExprFunction ZIG_LUAU_AST(ExprFunction_get)(Luau::AstExprFunction* value)
{
    zig_TypeList returnAnnotation;
    if (value->returnAnnotation.has_value())
    {
        returnAnnotation = {
            value->returnAnnotation.value().types.data, value->returnAnnotation.value().types.size, value->returnAnnotation.value().tailType
        };
    }

    zig_Location argLocation;
    if (value->argLocation.has_value())
    {
        argLocation = {
            {value->argLocation.value().begin.line, value->argLocation.value().begin.column},
            {value->argLocation.value().end.line, value->argLocation.value().end.column},
        };
    }

    return {
        value->attributes.data,
        value->attributes.size,
        value->generics.data,
        value->generics.size,
        value->genericPacks.data,
        value->genericPacks.size,
        value->self,
        value->args.data,
        value->args.size,
        value->returnAnnotation.has_value(),
        returnAnnotation,
        value->vararg,
        {
            {value->varargLocation.begin.line, value->varargLocation.begin.column},
            {value->varargLocation.end.line, value->varargLocation.end.column},
        },
        value->body,
        value->functionDepth,
        value->debugname.value,
        value->argLocation.has_value(),
        argLocation
    };
}

// ExprTable
EXPORT Luau::AstExprTable::Item* ZIG_LUAU_AST(ExprTable_get_items)(Luau::AstExprTable* value, size_t* len)
{
    *len = value->items.size;
    return value->items.data;
}

// StatLocal
EXPORT struct zig_StatLocal
{
    Luau::AstLocal** vars;
    size_t vars_len;
    Luau::AstExpr** values;
    size_t values_len;
    bool hasEqualsSignLocation;
    zig_Location equalsSignLocation;
};

EXPORT zig_StatLocal ZIG_LUAU_AST(StatLocal_get)(Luau::AstStatLocal* value)
{
    zig_Location equalsSignLocation;
    if (value->equalsSignLocation.has_value())
    {
        equalsSignLocation = {
            {value->equalsSignLocation.value().begin.line, value->equalsSignLocation.value().begin.column},
            {value->equalsSignLocation.value().end.line, value->equalsSignLocation.value().end.column},
        };
    }

    return {
        value->vars.data,
        value->vars.size,
        value->values.data,
        value->values.size,
        value->equalsSignLocation.has_value(),
        equalsSignLocation,
    };
}

// StatBlock
EXPORT const Luau::AstStat* const* ZIG_LUAU_AST(StatBlock_get_statements)(Luau::AstStatBlock* parent, size_t* len)
{
    *len = parent->body.size;
    return parent->body.data;
}