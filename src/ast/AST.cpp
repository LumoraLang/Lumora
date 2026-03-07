#include "lumora/AST.h"
#include <stdexcept>

namespace lumora::ast {

static void walkChildren(Node& n, ASTVisitorFn& pre, ASTVisitorFn& post);

void walkAST(Node& root, ASTVisitorFn pre, ASTVisitorFn post) {
    if (pre && !pre(root)) return;
    walkChildren(root, pre, post);
    if (post) post(root);
}

static void walk(NodePtr& p, ASTVisitorFn& pre, ASTVisitorFn& post) {
    if (p) walkAST(*p, pre, post);
}

static void walkChildren(Node& n, ASTVisitorFn& pre, ASTVisitorFn& post) {
    switch (n.kind) {
        case NodeKind::Module: {
            auto& m = static_cast<Module&>(n);
            for (auto& c : m.items) walk(c, pre, post);
            break;
        }
        case NodeKind::FnDecl: {
            auto& f = static_cast<FnDecl&>(n);
            for (auto& p : f.params) walk(reinterpret_cast<NodePtr&>(p), pre, post);
            walk(f.body, pre, post);
            break;
        }
        case NodeKind::BlockStmt: {
            auto& b = static_cast<BlockStmt&>(n);
            for (auto& s : b.stmts) walk(s, pre, post);
            break;
        }
        case NodeKind::LetStmt: {
            auto& l = static_cast<LetStmt&>(n);
            walk(l.init, pre, post);
            break;
        }
        case NodeKind::ReturnStmt: {
            walk(static_cast<ReturnStmt&>(n).value, pre, post);
            break;
        }
        case NodeKind::IfStmt: {
            auto& s = static_cast<IfStmt&>(n);
            walk(s.cond, pre, post);
            walk(s.thenBranch, pre, post);
            walk(s.elseBranch, pre, post);
            break;
        }
        case NodeKind::WhileStmt: {
            auto& s = static_cast<WhileStmt&>(n);
            walk(s.cond, pre, post);
            walk(s.body, pre, post);
            break;
        }
        case NodeKind::ForStmt: {
            auto& s = static_cast<ForStmt&>(n);
            walk(s.iter, pre, post);
            walk(s.body, pre, post);
            break;
        }
        case NodeKind::ExprStmt:
            walk(static_cast<ExprStmt&>(n).expr, pre, post);
            break;
        case NodeKind::DeferStmt:
            walk(static_cast<DeferStmt&>(n).expr, pre, post);
            break;
        case NodeKind::BinaryExpr: {
            auto& e = static_cast<BinaryExpr&>(n);
            walk(e.lhs, pre, post);
            walk(e.rhs, pre, post);
            break;
        }
        case NodeKind::UnaryExpr:
            walk(static_cast<UnaryExpr&>(n).operand, pre, post);
            break;
        case NodeKind::CallExpr: {
            auto& e = static_cast<CallExpr&>(n);
            walk(e.callee, pre, post);
            for (auto& a : e.args) walk(a, pre, post);
            break;
        }
        case NodeKind::IndexExpr: {
            auto& e = static_cast<IndexExpr&>(n);
            walk(e.obj, pre, post);
            walk(e.idx, pre, post);
            break;
        }
        case NodeKind::FieldExpr:
            walk(static_cast<FieldExpr&>(n).obj, pre, post);
            break;
        case NodeKind::CastExpr:
            walk(static_cast<CastExpr&>(n).obj, pre, post);
            break;
        case NodeKind::AssignExpr: {
            auto& e = static_cast<AssignExpr&>(n);
            walk(e.lhs, pre, post);
            walk(e.rhs, pre, post);
            break;
        }
        case NodeKind::MatchExpr: {
            auto& e = static_cast<MatchExpr&>(n);
            walk(e.subject, pre, post);
            for (auto& arm : e.arms) walk(reinterpret_cast<NodePtr&>(arm), pre, post);
            break;
        }
        case NodeKind::ArrayExpr: {
            auto& e = static_cast<ArrayExpr&>(n);
            for (auto& el : e.elems) walk(el, pre, post);
            walk(e.repeat, pre, post);
            break;
        }
        case NodeKind::StructExpr: {
            auto& e = static_cast<StructExpr&>(n);
            for (auto& f : e.fields) walk(reinterpret_cast<NodePtr&>(f), pre, post);
            break;
        }
        case NodeKind::ExtensionNode: {
            auto& e = static_cast<ExtensionNode&>(n);
            for (auto& c : e.children) walk(c, pre, post);
            break;
        }
        default:
            break;
    }
}

} 
