#pragma once
#include "lumora/Token.h"
#include <memory>
#include <vector>
#include <string>
#include <optional>
#include <variant>
#include <functional>

namespace lumora::ast {

struct Node;
using NodePtr  = std::unique_ptr<Node>;
using NodeList = std::vector<NodePtr>;

enum class NodeKind : uint32_t {
    Module,

    FnDecl, ParamDecl, StructDecl, FieldDecl,
    EnumDecl, EnumVariant, TraitDecl, ImplDecl,
    TypeAlias, UseDecl, ModDecl, ExternDecl,

    BlockStmt, LetStmt, ReturnStmt, IfStmt, WhileStmt,
    ForStmt, BreakStmt, ContinueStmt, ExprStmt, DeferStmt,

    BinaryExpr, UnaryExpr, CallExpr, IndexExpr, FieldExpr,
    CastExpr, AssignExpr, MatchExpr, MatchArm,
    ArrayExpr, StructExpr, StructExprField,
    LambdaExpr, SizeofExpr, AlignofExpr, TypeofExpr,
    OffsetofExpr,

    IntLit, FloatLit, StringLit, CharLit, BoolLit, NullLit,
    IdentExpr, PathExpr,

    PrimType, PtrType, RefType, SliceType, ArrayType,
    FnType, NamedType, TupleType,

    ExtensionNode,
};

struct Node {
    NodeKind  kind;
    SourceLoc loc;
    virtual ~Node() = default;
};

using TypePtr = std::unique_ptr<Node>;

struct Attribute {
    std::string        name;
    std::vector<Token> args;
    SourceLoc          loc;
};

template <NodeKind K>
struct TypedNode : Node {
    TypedNode() { kind = K; }
};

struct PrimTypeNode : TypedNode<NodeKind::PrimType> {
    enum class Prim { I8, I16, I32, I64, I128,
                      U8, U16, U32, U64, U128,
                      F32, F64, Bool, Char, Void, Never };
    Prim prim;
};

struct PtrTypeNode : TypedNode<NodeKind::PtrType> {
    bool    isMut = false;
    TypePtr inner;
};

struct RefTypeNode : TypedNode<NodeKind::RefType> {
    bool    isMut = false;
    TypePtr inner;
};

struct SliceTypeNode : TypedNode<NodeKind::SliceType> {
    TypePtr inner;
};

struct ArrayTypeNode : TypedNode<NodeKind::ArrayType> {
    TypePtr inner;
    NodePtr size;
};

struct FnTypeNode : TypedNode<NodeKind::FnType> {
    std::vector<TypePtr> params;
    TypePtr              ret;
    bool                 isVararg = false;
};

struct NamedTypeNode : TypedNode<NodeKind::NamedType> {
    std::vector<std::string>  path;
    std::vector<TypePtr>      typeArgs;
};

struct TupleTypeNode : TypedNode<NodeKind::TupleType> {
    std::vector<TypePtr> elems;
};

struct ParamDecl : TypedNode<NodeKind::ParamDecl> {
    std::string name;
    TypePtr     ty;
    NodePtr     defaultVal;
    bool        isMut     = false;
    bool        isVararg  = false;
};

struct FnDecl : TypedNode<NodeKind::FnDecl> {
    std::string              name;
    std::vector<std::unique_ptr<ParamDecl>> params;
    TypePtr                  retTy;
    NodePtr                  body;
    std::vector<Attribute>   attrs;
    std::vector<std::string> generics;
    bool                     isPub    = false;
    bool                     isExtern = false;
    bool                     isInline = false;
    bool                     isAsync  = false;
    std::optional<std::string> linkage;
};

struct FieldDecl : TypedNode<NodeKind::FieldDecl> {
    std::string            name;
    TypePtr                ty;
    NodePtr                defaultVal;
    std::vector<Attribute> attrs;
    bool                   isPub = false;
};

struct StructDecl : TypedNode<NodeKind::StructDecl> {
    std::string                            name;
    std::vector<std::unique_ptr<FieldDecl>> fields;
    std::vector<Attribute>                 attrs;
    std::vector<std::string>               generics;
    bool                                   isPub = false;
};

struct EnumVariant : TypedNode<NodeKind::EnumVariant> {
    std::string             name;
    std::vector<TypePtr>    fields;
    NodePtr                 discriminant;
};

struct EnumDecl : TypedNode<NodeKind::EnumDecl> {
    std::string                                name;
    std::vector<std::unique_ptr<EnumVariant>>  variants;
    std::vector<Attribute>                     attrs;
    bool                                       isPub = false;
};

struct TraitDecl : TypedNode<NodeKind::TraitDecl> {
    std::string  name;
    NodeList     items;
    bool         isPub = false;
};

struct ImplDecl : TypedNode<NodeKind::ImplDecl> {
    TypePtr       ty;
    std::optional<TypePtr> traitTy;
    NodeList      items;
};

struct TypeAlias : TypedNode<NodeKind::TypeAlias> {
    std::string name;
    TypePtr     ty;
    bool        isPub = false;
};

struct UseDecl : TypedNode<NodeKind::UseDecl> {
    std::vector<std::string> path;
    std::optional<std::string> alias;
    bool                       isPub = false;
};

struct ModDecl : TypedNode<NodeKind::ModDecl> {
    std::string name;
    NodeList    items;
    bool        isPub = false;
};

struct ExternDecl : TypedNode<NodeKind::ExternDecl> {
    std::string abi;
    NodeList    items;
};

struct Module : TypedNode<NodeKind::Module> {
    std::string file;
    NodeList    items;
};

struct BlockStmt : TypedNode<NodeKind::BlockStmt> {
    NodeList stmts;
};

struct LetStmt : TypedNode<NodeKind::LetStmt> {
    std::string name;
    TypePtr     ty;
    NodePtr     init;
    bool        isMut  = false;
    bool        isPub  = false;
    bool        isConst = false;
};

struct ReturnStmt : TypedNode<NodeKind::ReturnStmt> {
    NodePtr value;
};

struct IfStmt : TypedNode<NodeKind::IfStmt> {
    NodePtr cond;
    NodePtr thenBranch;
    NodePtr elseBranch;
};

struct WhileStmt : TypedNode<NodeKind::WhileStmt> {
    NodePtr cond;
    NodePtr body;
    std::optional<std::string> label;
};

struct ForStmt : TypedNode<NodeKind::ForStmt> {
    std::string var;
    NodePtr     iter;
    NodePtr     body;
    std::optional<std::string> label;
};

struct BreakStmt    : TypedNode<NodeKind::BreakStmt>    { std::optional<std::string> label; };
struct ContinueStmt : TypedNode<NodeKind::ContinueStmt> { std::optional<std::string> label; };

struct ExprStmt : TypedNode<NodeKind::ExprStmt> { NodePtr expr; };
struct DeferStmt : TypedNode<NodeKind::DeferStmt> { NodePtr expr; };

struct BinaryExpr : TypedNode<NodeKind::BinaryExpr> {
    TokenKind op;
    NodePtr   lhs;
    NodePtr   rhs;
};

struct UnaryExpr : TypedNode<NodeKind::UnaryExpr> {
    TokenKind op;
    NodePtr   operand;
    bool      isPostfix = false;
};

struct CallExpr : TypedNode<NodeKind::CallExpr> {
    NodePtr              callee;
    std::vector<NodePtr> args;
    std::vector<TypePtr> typeArgs;
};

struct IndexExpr : TypedNode<NodeKind::IndexExpr> {
    NodePtr obj;
    NodePtr idx;
};

struct FieldExpr : TypedNode<NodeKind::FieldExpr> {
    NodePtr     obj;
    std::string field;
};

struct CastExpr : TypedNode<NodeKind::CastExpr> {
    NodePtr obj;
    TypePtr ty;
};

struct AssignExpr : TypedNode<NodeKind::AssignExpr> {
    TokenKind op;
    NodePtr   lhs;
    NodePtr   rhs;
};

struct MatchArm : TypedNode<NodeKind::MatchArm> {
    NodeList patterns;
    NodePtr  guard;
    NodePtr  body;
};

struct MatchExpr : TypedNode<NodeKind::MatchExpr> {
    NodePtr                          subject;
    std::vector<std::unique_ptr<MatchArm>> arms;
};

struct ArrayExpr : TypedNode<NodeKind::ArrayExpr> {
    std::vector<NodePtr> elems;
    NodePtr              repeat;
};

struct StructExprField : TypedNode<NodeKind::StructExprField> {
    std::string name;
    NodePtr     value;
};

struct StructExpr : TypedNode<NodeKind::StructExpr> {
    std::vector<std::string>                   path;
    std::vector<std::unique_ptr<StructExprField>> fields;
};

struct LambdaExpr : TypedNode<NodeKind::LambdaExpr> {
    std::vector<std::unique_ptr<ParamDecl>> params;
    TypePtr                                 retTy;
    NodePtr                                 body;
};

struct SizeofExpr   : TypedNode<NodeKind::SizeofExpr>   { TypePtr ty; };
struct AlignofExpr  : TypedNode<NodeKind::AlignofExpr>  { TypePtr ty; };
struct TypeofExpr   : TypedNode<NodeKind::TypeofExpr>   { NodePtr expr; };

struct OffsetofExpr : TypedNode<NodeKind::OffsetofExpr> {
    TypePtr     ty;
    std::string field;
};

struct IntLit    : TypedNode<NodeKind::IntLit>    { int64_t     value = 0; };
struct FloatLit  : TypedNode<NodeKind::FloatLit>  { double      value = 0.0; };
struct StringLit : TypedNode<NodeKind::StringLit> { std::string value; };
struct CharLit   : TypedNode<NodeKind::CharLit>   { char        value = 0; };
struct BoolLit   : TypedNode<NodeKind::BoolLit>   { bool        value = false; };
struct NullLit   : TypedNode<NodeKind::NullLit>   {};

struct IdentExpr : TypedNode<NodeKind::IdentExpr> { std::string name; };

struct PathExpr : TypedNode<NodeKind::PathExpr> {
    std::vector<std::string> segments;
    std::vector<TypePtr>     typeArgs;
};

struct ExtensionNode : TypedNode<NodeKind::ExtensionNode> {
    std::string              extensionId;
    std::vector<Token>       tokens;
    std::vector<NodePtr>     children;
    std::string              irFragment;
};

using ASTVisitorFn = std::function<bool(Node&)>;
void walkAST(Node& root, ASTVisitorFn pre, ASTVisitorFn post = {});

} 
