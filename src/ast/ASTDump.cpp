#include "lumora/ASTDump.h"
#include "lumora/AST.h"
#include <iostream>
#include <string>
namespace lumora::ast {
static void dumpNode(const Node& n, int depth, std::ostream& out);
static std::string indent(int d) { return std::string(d * 2, ' '); }
static void dumpChildren(const Node& n, int depth, std::ostream& out) {
    switch (n.kind) {
        case NodeKind::Module:
            for (auto& c : static_cast<const Module&>(n).items) dumpNode(*c, depth, out);
            break;
        case NodeKind::FnDecl: {
            auto& f = static_cast<const FnDecl&>(n);
            for (auto& p : f.params) dumpNode(*p, depth, out);
            if (f.body) dumpNode(*f.body, depth, out);
            break;
        }
        case NodeKind::BlockStmt:
            for (auto& s : static_cast<const BlockStmt&>(n).stmts) dumpNode(*s, depth, out);
            break;
        case NodeKind::IfStmt: {
            auto& s = static_cast<const IfStmt&>(n);
            if (s.cond)       dumpNode(*s.cond,       depth, out);
            if (s.thenBranch) dumpNode(*s.thenBranch, depth, out);
            if (s.elseBranch) dumpNode(*s.elseBranch, depth, out);
            break;
        }
        case NodeKind::BinaryExpr: {
            auto& e = static_cast<const BinaryExpr&>(n);
            dumpNode(*e.lhs, depth, out);
            dumpNode(*e.rhs, depth, out);
            break;
        }
        case NodeKind::CallExpr: {
            auto& e = static_cast<const CallExpr&>(n);
            dumpNode(*e.callee, depth, out);
            for (auto& a : e.args) dumpNode(*a, depth, out);
            break;
        }
        case NodeKind::AsmExpr: {
            auto& e = static_cast<const AsmExpr&>(n);
            for (auto& o : e.outputs) if (o.expr) dumpNode(*o.expr, depth, out);
            for (auto& i : e.inputs)  if (i.expr) dumpNode(*i.expr, depth, out);
            break;
        }
        default: break;
    }
}

static void dumpNode(const Node& n, int depth, std::ostream& out) {
    out << indent(depth);
    switch (n.kind) {
        case NodeKind::Module:       out << "Module\n"; break;
        case NodeKind::FnDecl:       out << "FnDecl:" << static_cast<const FnDecl&>(n).name << "\n"; break;
        case NodeKind::ParamDecl:    out << "Param:"  << static_cast<const ParamDecl&>(n).name << "\n"; break;
        case NodeKind::StructDecl:   out << "Struct:" << static_cast<const StructDecl&>(n).name << "\n"; break;
        case NodeKind::BlockStmt:    out << "Block\n"; break;
        case NodeKind::LetStmt:      out << "Let:" << static_cast<const LetStmt&>(n).name << "\n"; break;
        case NodeKind::ReturnStmt:   out << "Return\n"; break;
        case NodeKind::IfStmt:       out << "If\n"; break;
        case NodeKind::WhileStmt:    out << "While\n"; break;
        case NodeKind::ForStmt:      out << "For:" << static_cast<const ForStmt&>(n).var << "\n"; break;
        case NodeKind::BinaryExpr:   out << "Binary:" << tokenKindName(static_cast<const BinaryExpr&>(n).op) << "\n"; break;
        case NodeKind::UnaryExpr:    out << "Unary:" << tokenKindName(static_cast<const UnaryExpr&>(n).op) << "\n"; break;
        case NodeKind::CallExpr:     out << "Call\n"; break;
        case NodeKind::IdentExpr:    out << "Ident:" << static_cast<const IdentExpr&>(n).name << "\n"; break;
        case NodeKind::IntLit:       out << "Int:"   << static_cast<const IntLit&>(n).value   << "\n"; break;
        case NodeKind::FloatLit:     out << "Float:" << static_cast<const FloatLit&>(n).value  << "\n"; break;
        case NodeKind::StringLit:    out << "Str:\"" << static_cast<const StringLit&>(n).value << "\"\n"; break;
        case NodeKind::BoolLit:      out << "Bool:" << (static_cast<const BoolLit&>(n).value ? "true" : "false") << "\n"; break;
        case NodeKind::NullLit:      out << "Null\n"; break;
        case NodeKind::ExtensionNode: out << "Ext:" << static_cast<const ExtensionNode&>(n).extensionId << "\n"; break;
        case NodeKind::AsmExpr:       out << "Asm:" << (static_cast<const AsmExpr&>(n).isBlock ? "block" : "extended") << "\n"; break;
        default:                     out << "Node(" << static_cast<int>(n.kind) << ")\n"; break;
    }
    dumpChildren(n, depth + 1, out);
}

void dumpAST(const Node& root, std::ostream& out) {
    dumpNode(root, 0, out);
}
}
