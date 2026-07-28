#include "lumora/IREmitter.h"
#include <format>
#include <stdexcept>
namespace lumora {
std::string IREmitter::llvmType(const SemaType& t) {
    switch (t.kind) {
        case TypeKind::Void:    return "void";
        case TypeKind::Never:   return "void";
        case TypeKind::Bool:    return "i1";
        case TypeKind::Char:    return "i8";
        case TypeKind::I8:      return "i8";
        case TypeKind::I16:     return "i16";
        case TypeKind::I32:     return "i32";
        case TypeKind::I64:     return "i64";
        case TypeKind::I128:    return "i128";
        case TypeKind::U8:      return "i8";
        case TypeKind::U16:     return "i16";
        case TypeKind::U32:     return "i32";
        case TypeKind::U64:     return "i64";
        case TypeKind::U128:    return "i128";
        case TypeKind::F32:     return "float";
        case TypeKind::F64:     return "double";
        case TypeKind::Pointer:
        case TypeKind::Reference:
            if (t.inner) {
                auto innerTy = llvmType(*t.inner);
                if (innerTy == "void") return "i8*";
                return innerTy + "*";
            }
            return "i8*";
        case TypeKind::Slice:
            if (t.inner) {
                auto elemTy = llvmType(*t.inner);
                return "{ " + elemTy + "*, i64 }";
            }
            return "{ i8*, i64 }";
        case TypeKind::Array:
            if (t.inner)
                return std::format("[{} x {}]", t.arrayLen, llvmType(*t.inner));
            return "[0 x i8]";
        case TypeKind::Tuple: {
            std::string s = "{ ";
            for (size_t i = 0; i < t.params.size(); ++i) {
                if (i) s += ", ";
                s += llvmType(*t.params[i]);
            }
            return s + " }";
        }
        case TypeKind::Struct:
        case TypeKind::Named:
            return "%struct." + t.name;
        case TypeKind::Enum: {
            bool hasPayload = false;
            for (auto &v : t.params) {
                if (!v->params.empty()) {
                    hasPayload = true;
                    break;
                }
            }
            if (!hasPayload) return "i32";
            return "{ i8, [4 x i8] }";
        }
        case TypeKind::Fn: {
            std::string ret = t.ret ? llvmType(*t.ret) : "void";
            std::string ps;
            for (size_t i = 0; i < t.params.size(); ++i) {
                if (i) ps += ", ";
                ps += llvmType(*t.params[i]);
            }
            if (t.isVararg) { if (!ps.empty()) ps += ", "; ps += "..."; }
            return ret + " (" + ps + ")*";
        }
        default:
            return "i8*";
    }
}

std::string IREmitter::llvmType(TypeRef t) {
    if (!t) return "void";
    return llvmType(*t);
}

std::string IREmitter::llvmTypeAST(ast::Node* tyNode) {
    if (!tyNode) return "void";
    auto resolved = m_sema.resolveType(*tyNode);
    return llvmType(resolved);
}
}
