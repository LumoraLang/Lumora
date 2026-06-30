#pragma once
#include "lumora/AST.h"
#include <unordered_map>
#include <vector>
#include <string>
#include <memory>
#include <optional>
#include <functional>
namespace lumora {
enum class TypeKind {
    Void, Never, Bool, Char,
    I8, I16, I32, I64, I128,
    U8, U16, U32, U64, U128,
    F32, F64,
    Pointer, Reference, Slice, Array, Tuple,
    Struct, Enum, Trait,
    Fn, Named, Generic,
    Extension,
};

struct SemaType;
using TypeRef = std::shared_ptr<SemaType>;
struct SemaType {
    TypeKind                  kind;
    std::string               name;
    TypeRef                   inner;
    std::vector<TypeRef>      params;
    TypeRef                   ret;
    size_t                    arrayLen = 0;
    bool                      isMut    = false;
    bool                      isVararg = false;
    static TypeRef voidTy();
    static TypeRef neverTy();
    static TypeRef boolTy();
    static TypeRef charTy();
    static TypeRef i32Ty();
    static TypeRef i64Ty();
    static TypeRef u8Ty();
    static TypeRef f64Ty();
    static TypeRef ptrTy(TypeRef inner, bool isMut = false);
    static TypeRef refTy(TypeRef inner, bool isMut = false);
    [[nodiscard]] bool isInt()    const noexcept;
    [[nodiscard]] bool isFloat()  const noexcept;
    [[nodiscard]] bool isNumeric()const noexcept;
    [[nodiscard]] bool isPtr()    const noexcept;
    [[nodiscard]] bool isString() const noexcept;
    [[nodiscard]] bool isBool()   const noexcept;
    [[nodiscard]] std::string str() const;
};

struct Symbol {
    std::string name;
    TypeRef     type;
    bool        isMut = false;
    bool        isFn  = false;
    ast::Node*  decl  = nullptr;
};

struct Scope {
    std::unordered_map<std::string, Symbol> syms;
    Scope* parent = nullptr;
    std::optional<Symbol> lookup(std::string_view name) const;
    void define(Symbol sym);
};

struct SemaError {
    std::string message;
    SourceLoc   loc;
};

struct SemaExtensionPoint {
    std::string                                extensionId;
    std::function<TypeRef(ast::ExtensionNode&, class Sema&)> handler;
};

class Sema {
public:
    Sema();
    void registerExtension(SemaExtensionPoint ext);
    bool analyze(ast::Module& mod);
    [[nodiscard]] const std::vector<SemaError>& errors() const noexcept;
    [[nodiscard]] bool hasErrors()                        const noexcept;
    TypeRef        resolveType(ast::Node& tyNode);
    TypeRef        inferExpr(ast::Node& expr);
    void           pushScope();
    void           popScope();
    Scope&         currentScope();
    std::optional<Symbol> lookupSymbol(std::string_view name);

private:
    std::vector<Scope>                 m_scopes;
    std::vector<SemaError>             m_errors;
    std::vector<SemaExtensionPoint>    m_extensions;
    TypeRef                            m_currentReturnType;
    void    error(std::string_view msg, SourceLoc loc);
    void    analyzeItem(ast::Node& n);
    void    analyzeFn(ast::FnDecl& fn);
    void    analyzeStruct(ast::StructDecl& s);
    void    analyzeBlock(ast::BlockStmt& b);
    void    analyzeStmt(ast::Node& n);
    TypeRef analyzeExpr(ast::Node& n);
    TypeRef primToSema(ast::PrimTypeNode::Prim p);
    TypeRef resolveTypeNode(ast::Node& n);
    std::unordered_map<std::string, TypeRef> m_typeEnv;
};

}
