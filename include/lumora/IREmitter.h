#pragma once
#include "lumora/AST.h"
#include "lumora/Sema.h"
#include <string>
#include <sstream>
#include <unordered_map>
#include <unordered_set>
#include <vector>
#include <functional>
#include <optional>

namespace lumora {

struct IRValue {
    std::string reg;
    TypeRef     type;
    bool        isPtr = false;
};

struct IRBlock {
    std::string         label;
    std::vector<std::string> instrs;
};

struct IRFunction {
    std::string              name;
    std::string              retTyStr;
    std::vector<std::string> paramStrs;
    std::vector<IRBlock>     blocks;
    bool                     isDecl   = false;
    bool                     isVararg = false;
};

struct IRGlobal {
    std::string name;
    std::string tyStr;
    std::string initVal;
    bool        isConst = false;
};

struct CodegenExtensionPoint {
    std::string extensionId;
    std::function<std::string(ast::ExtensionNode&, class IREmitter&)> handler;
};

class IREmitter {
public:
    explicit IREmitter(Sema& sema);

    void registerExtension(CodegenExtensionPoint ext);

    std::string emit(ast::Module& mod);

    std::string newReg();
    std::string newLabel(std::string_view hint = "bb");

    void        emitInstr(std::string instr);
    void        emitRaw(std::string raw);

    std::string llvmType(const SemaType& t);
    std::string llvmType(TypeRef t);
    std::string llvmTypeAST(ast::Node* tyNode);

    IRValue     emitExpr(ast::Node& n);
    void        beginBlock(std::string label);
    void        emitToCurrentBlock(std::string instr);

private:
    Sema&                                m_sema;
    std::vector<CodegenExtensionPoint>   m_extensions;

    std::ostringstream                   m_out;
    uint32_t                             m_regCounter   = 0;
    uint32_t                             m_labelCounter = 0;
    uint32_t                             m_strCounter   = 0;

    std::string                          m_currentFn;
    std::string                          m_currentRetTy;
    std::vector<IRBlock>                 m_blocks;
    size_t                               m_currentBlock = 0;

    std::unordered_map<std::string, IRValue>  m_locals;
    std::unordered_map<std::string, IRValue>  m_globals;
    std::vector<std::string>                  m_stringLits;
    std::unordered_set<std::string>           m_declaredFns;
    std::vector<ast::Node*>                   m_deferStack;

    void        emitModule(ast::Module& mod);
    void        emitTopLevel(ast::Node& n);
    void        emitFnDecl(ast::FnDecl& fn);
    void        emitExternDecl(ast::ExternDecl& ext);
    void        emitStructDecl(ast::StructDecl& s);
    void        emitEnumDecl(ast::EnumDecl& e);
    void        emitGlobalLet(ast::LetStmt& l);

    void        emitBlock(ast::BlockStmt& b);
    void        emitStmt(ast::Node& n);
    void        emitLetStmt(ast::LetStmt& l);
    void        emitReturnStmt(ast::ReturnStmt& r);
    void        emitIfStmt(ast::IfStmt& s);
    void        emitWhileStmt(ast::WhileStmt& s);
    void        emitForStmt(ast::ForStmt& s);
    void        emitDeferStmt(ast::DeferStmt& s);

    IRValue     emitBinaryExpr(ast::BinaryExpr& e);
    IRValue     emitUnaryExpr(ast::UnaryExpr& e);
    IRValue     emitCallExpr(ast::CallExpr& e);
    IRValue     emitIdentExpr(ast::IdentExpr& e);
    IRValue     emitIntLit(ast::IntLit& e);
    IRValue     emitFloatLit(ast::FloatLit& e);
    IRValue     emitStringLit(ast::StringLit& e);
    IRValue     emitBoolLit(ast::BoolLit& e);
    IRValue     emitNullLit(ast::NullLit& e);
    IRValue     emitCastExpr(ast::CastExpr& e);
    IRValue     emitAssignExpr(ast::AssignExpr& e);
    IRValue     emitFieldExpr(ast::FieldExpr& e);
    IRValue     emitIndexExpr(ast::IndexExpr& e);
    IRValue     emitStructExpr(ast::StructExpr& e);

    IRValue     emitExtensionNode(ast::ExtensionNode& n);
    IRValue     emitAsmExpr(ast::AsmExpr& n);

    IRValue     load(const IRValue& ptr);
    void        store(const IRValue& val, const IRValue& ptr);
    IRValue     allocaLocal(TypeRef ty, const std::string& hint = "");

    std::string intOp(TokenKind op, bool isSigned) const noexcept;
    std::string floatOp(TokenKind op) const noexcept;
    std::string icmpOp(TokenKind op, bool isSigned) const noexcept;
    std::string fcmpOp(TokenKind op) const noexcept;

    std::string currentLabel() const;

    void        flushFn(const std::string& name, const std::string& retTy,
                        const std::vector<std::string>& paramStrs, bool isVararg);
    void        ensureDeclared(const std::string& decl);
    void        emitDeferred();
};

} 
