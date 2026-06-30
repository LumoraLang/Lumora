#include "lumora/Sema.h"
namespace lumora {
using namespace ast;
TypeRef SemaType::voidTy() {
  auto t = std::make_shared<SemaType>();
  t->kind = TypeKind::Void;
  t->name = "void";
  return t;
}
TypeRef SemaType::neverTy() {
  auto t = std::make_shared<SemaType>();
  t->kind = TypeKind::Never;
  t->name = "never";
  return t;
}
TypeRef SemaType::boolTy() {
  auto t = std::make_shared<SemaType>();
  t->kind = TypeKind::Bool;
  t->name = "bool";
  return t;
}
TypeRef SemaType::charTy() {
  auto t = std::make_shared<SemaType>();
  t->kind = TypeKind::Char;
  t->name = "char";
  return t;
}
TypeRef SemaType::i32Ty() {
  auto t = std::make_shared<SemaType>();
  t->kind = TypeKind::I32;
  t->name = "i32";
  return t;
}
TypeRef SemaType::i64Ty() {
  auto t = std::make_shared<SemaType>();
  t->kind = TypeKind::I64;
  t->name = "i64";
  return t;
}
TypeRef SemaType::u8Ty() {
  auto t = std::make_shared<SemaType>();
  t->kind = TypeKind::U8;
  t->name = "u8";
  return t;
}
TypeRef SemaType::f64Ty() {
  auto t = std::make_shared<SemaType>();
  t->kind = TypeKind::F64;
  t->name = "f64";
  return t;
}

TypeRef SemaType::ptrTy(TypeRef inner, bool isMut) {
  auto t = std::make_shared<SemaType>();
  t->kind = TypeKind::Pointer;
  t->inner = std::move(inner);
  t->isMut = isMut;
  t->name = isMut ? "*mut " + t->inner->name : "*" + t->inner->name;
  return t;
}

TypeRef SemaType::refTy(TypeRef inner, bool isMut) {
  auto t = std::make_shared<SemaType>();
  t->kind = TypeKind::Reference;
  t->inner = std::move(inner);
  t->isMut = isMut;
  t->name = isMut ? "&mut " + t->inner->name : "&" + t->inner->name;
  return t;
}

bool SemaType::isInt() const noexcept {
  return kind >= TypeKind::I8 && kind <= TypeKind::U128;
}
bool SemaType::isFloat() const noexcept {
  return kind == TypeKind::F32 || kind == TypeKind::F64;
}
bool SemaType::isNumeric() const noexcept { return isInt() || isFloat(); }
bool SemaType::isPtr() const noexcept {
  return kind == TypeKind::Pointer || kind == TypeKind::Reference;
}
bool SemaType::isBool() const noexcept { return kind == TypeKind::Bool; }
bool SemaType::isString() const noexcept {
  return (kind == TypeKind::Pointer || kind == TypeKind::Reference) &&
         inner && inner->kind == TypeKind::U8;
}

std::string SemaType::str() const { return name; }

std::optional<Symbol> Scope::lookup(std::string_view name) const {
  auto it = syms.find(std::string(name));
  if (it != syms.end())
    return it->second;
  if (parent)
    return parent->lookup(name);
  return std::nullopt;
}

void Scope::define(Symbol sym) { syms[sym.name] = std::move(sym); }
Sema::Sema() {
  m_scopes.push_back({});
  m_typeEnv["void"] = SemaType::voidTy();
  m_typeEnv["never"] = SemaType::neverTy();
  m_typeEnv["bool"] = SemaType::boolTy();
  m_typeEnv["char"] = SemaType::charTy();
  m_typeEnv["i32"] = SemaType::i32Ty();
  m_typeEnv["i64"] = SemaType::i64Ty();
  m_typeEnv["u8"] = SemaType::u8Ty();
  m_typeEnv["f64"] = SemaType::f64Ty();
  for (auto &[n, k] : std::initializer_list<std::pair<const char *, TypeKind>>{
           {"i8", TypeKind::I8},
           {"i16", TypeKind::I16},
           {"i128", TypeKind::I128},
           {"u16", TypeKind::U16},
           {"u32", TypeKind::U32},
           {"u64", TypeKind::U64},
           {"u128", TypeKind::U128},
           {"f32", TypeKind::F32},
       }) {
    auto t = std::make_shared<SemaType>();
    t->kind = k;
    t->name = n;
    m_typeEnv[n] = t;
  }
}

void Sema::registerExtension(SemaExtensionPoint ext) {
  m_extensions.push_back(std::move(ext));
}

const std::vector<SemaError> &Sema::errors() const noexcept { return m_errors; }
bool Sema::hasErrors() const noexcept { return !m_errors.empty(); }
void Sema::error(std::string_view msg, SourceLoc loc) {
  m_errors.push_back({std::string(msg), loc});
}

void Sema::pushScope() {
  Scope s;
  s.parent = &m_scopes.back();
  m_scopes.push_back(std::move(s));
}

void Sema::popScope() {
  if (m_scopes.size() > 1)
    m_scopes.pop_back();
}

Scope &Sema::currentScope() { return m_scopes.back(); }
std::optional<Symbol> Sema::lookupSymbol(std::string_view name) {
  return m_scopes.back().lookup(name);
}

bool Sema::analyze(Module &mod) {
  for (auto &item : mod.items) {
    if (item)
      analyzeItem(*item);
  }
  return !hasErrors();
}

void Sema::analyzeItem(Node &n) {
  switch (n.kind) {
  case NodeKind::FnDecl:
    analyzeFn(static_cast<FnDecl &>(n));
    break;
  case NodeKind::StructDecl:
    analyzeStruct(static_cast<StructDecl &>(n));
    break;
  case NodeKind::LetStmt: {
    auto &l = static_cast<LetStmt &>(n);
    TypeRef ty = l.ty ? resolveTypeNode(*l.ty) : nullptr;
    if (l.init) {
      auto initTy = analyzeExpr(*l.init);
      if (!ty)
        ty = initTy;
    }
    if (!ty)
      ty = SemaType::voidTy();
    currentScope().define(Symbol{l.name, ty, l.isMut, false, &l});
    break;
  }
  case NodeKind::ExternDecl: {
    auto &ext = static_cast<ExternDecl &>(n);
    for (auto &item : ext.items) {
      if (item)
        analyzeItem(*item);
    }
    break;
  }
  case NodeKind::ExtensionNode:
    analyzeExpr(n);
    break;
  default:
    break;
  }
}

TypeRef Sema::primToSema(PrimTypeNode::Prim p) {
  switch (p) {
  case PrimTypeNode::Prim::I8:
    return m_typeEnv["i8"];
  case PrimTypeNode::Prim::I16:
    return m_typeEnv["i16"];
  case PrimTypeNode::Prim::I32:
    return m_typeEnv["i32"];
  case PrimTypeNode::Prim::I64:
    return m_typeEnv["i64"];
  case PrimTypeNode::Prim::I128:
    return m_typeEnv["i128"];
  case PrimTypeNode::Prim::U8:
    return m_typeEnv["u8"];
  case PrimTypeNode::Prim::U16:
    return m_typeEnv["u16"];
  case PrimTypeNode::Prim::U32:
    return m_typeEnv["u32"];
  case PrimTypeNode::Prim::U64:
    return m_typeEnv["u64"];
  case PrimTypeNode::Prim::U128:
    return m_typeEnv["u128"];
  case PrimTypeNode::Prim::F32:
    return m_typeEnv["f32"];
  case PrimTypeNode::Prim::F64:
    return m_typeEnv["f64"];
  case PrimTypeNode::Prim::Bool:
    return m_typeEnv["bool"];
  case PrimTypeNode::Prim::Char:
    return m_typeEnv["char"];
  case PrimTypeNode::Prim::Void:
    return m_typeEnv["void"];
  case PrimTypeNode::Prim::Never:
    return m_typeEnv["never"];
  default:
    return m_typeEnv["void"];
  }
}

TypeRef Sema::resolveTypeNode(Node &n) {
  switch (n.kind) {
  case NodeKind::PrimType:
    return primToSema(static_cast<PrimTypeNode &>(n).prim);
  case NodeKind::PtrType: {
    auto &p = static_cast<PtrTypeNode &>(n);
    auto inner = p.inner ? resolveTypeNode(*p.inner) : SemaType::voidTy();
    return SemaType::ptrTy(inner, p.isMut);
  }
  case NodeKind::RefType: {
    auto &r = static_cast<RefTypeNode &>(n);
    auto inner = r.inner ? resolveTypeNode(*r.inner) : SemaType::voidTy();
    return SemaType::refTy(inner, r.isMut);
  }
  case NodeKind::NamedType: {
    auto &named = static_cast<NamedTypeNode &>(n);
    if (!named.path.empty()) {
      auto it = m_typeEnv.find(named.path.back());
      if (it != m_typeEnv.end())
        return it->second;
      auto t = std::make_shared<SemaType>();
      t->kind = TypeKind::Named;
      t->name = named.path.back();
      return t;
    }
    return SemaType::voidTy();
  }
  default:
    return SemaType::voidTy();
  }
}

TypeRef Sema::resolveType(Node &n) { return resolveTypeNode(n); }
void Sema::analyzeFn(FnDecl &fn) {
  auto retType = fn.retTy ? resolveTypeNode(*fn.retTy) : SemaType::voidTy();
  auto fnType = std::make_shared<SemaType>();
  fnType->kind = TypeKind::Fn;
  fnType->name = fn.name;
  fnType->ret = retType;
  currentScope().define(Symbol{fn.name, fnType, false, true, &fn});
  pushScope();
  m_currentReturnType = retType;
  for (auto &p : fn.params) {
    auto paramTy = p->ty ? resolveTypeNode(*p->ty) : SemaType::voidTy();
    currentScope().define(Symbol{p->name, paramTy, p->isMut, false, p.get()});
    fnType->params.push_back(paramTy);
  }

  if (fn.body)
    analyzeBlock(static_cast<BlockStmt &>(*fn.body));

  popScope();
}

void Sema::analyzeStruct(StructDecl &s) {
  auto t = std::make_shared<SemaType>();
  t->kind = TypeKind::Struct;
  t->name = s.name;
  for (auto &f : s.fields) {
    t->fieldNames.push_back(f->name);
    t->params.push_back(f->ty ? resolveTypeNode(*f->ty) : SemaType::voidTy());
  }
  m_typeEnv[s.name] = t;
  currentScope().define(Symbol{s.name, t, false, false, &s});
}

void Sema::analyzeBlock(BlockStmt &b) {
  pushScope();
  for (auto &stmt : b.stmts) {
    if (stmt)
      analyzeStmt(*stmt);
  }
  popScope();
}

void Sema::analyzeStmt(Node &n) {
  switch (n.kind) {
  case NodeKind::LetStmt: {
    auto &l = static_cast<LetStmt &>(n);
    TypeRef ty = l.ty ? resolveTypeNode(*l.ty) : nullptr;
    if (l.init) {
      auto initTy = analyzeExpr(*l.init);
      if (!ty)
        ty = initTy;
    }
    if (!ty)
      ty = SemaType::voidTy();
    currentScope().define(Symbol{l.name, ty, l.isMut, false, &l});
    break;
  }
  case NodeKind::ReturnStmt: {
    auto &r = static_cast<ReturnStmt &>(n);
    if (r.value)
      analyzeExpr(*r.value);
    break;
  }
  case NodeKind::IfStmt: {
    auto &s = static_cast<IfStmt &>(n);
    if (s.cond)
      analyzeExpr(*s.cond);
    if (s.thenBranch)
      analyzeStmt(*s.thenBranch);
    if (s.elseBranch)
      analyzeStmt(*s.elseBranch);
    break;
  }
  case NodeKind::WhileStmt: {
    auto &s = static_cast<WhileStmt &>(n);
    if (s.cond)
      analyzeExpr(*s.cond);
    if (s.body)
      analyzeStmt(*s.body);
    break;
  }
  case NodeKind::ForStmt: {
    auto &s = static_cast<ForStmt &>(n);
    if (s.iter) {
      auto iterTy = analyzeExpr(*s.iter);
      pushScope();
      currentScope().define(Symbol{s.var, iterTy, true, false, &s});
      if (s.body)
        analyzeStmt(*s.body);
      popScope();
    }
    break;
  }
  case NodeKind::BlockStmt:
    analyzeBlock(static_cast<BlockStmt &>(n));
    break;
  case NodeKind::ExprStmt:
    analyzeExpr(*static_cast<ExprStmt &>(n).expr);
    break;
  case NodeKind::DeferStmt:
    analyzeExpr(*static_cast<DeferStmt &>(n).expr);
    break;
  default:
    break;
  }
}

TypeRef Sema::analyzeExpr(Node &n) {
  switch (n.kind) {
  case NodeKind::IntLit:
    return SemaType::i64Ty();
  case NodeKind::FloatLit:
    return SemaType::f64Ty();
  case NodeKind::BoolLit:
    return SemaType::boolTy();
  case NodeKind::CharLit:
    return SemaType::charTy();
  case NodeKind::NullLit:
    return SemaType::ptrTy(SemaType::voidTy());
  case NodeKind::StringLit: {
    auto t = std::make_shared<SemaType>();
    t->kind = TypeKind::Pointer;
    t->inner = SemaType::u8Ty();
    t->name = "*u8";
    return t;
  }
  case NodeKind::IdentExpr: {
    auto &id = static_cast<IdentExpr &>(n);
    auto sym = lookupSymbol(id.name);
    if (!sym) {
      return SemaType::voidTy();
    }
    return sym->type;
  }
  case NodeKind::BinaryExpr: {
    auto &b = static_cast<BinaryExpr &>(n);
    auto lty = analyzeExpr(*b.lhs);
    auto rty = analyzeExpr(*b.rhs);
    if (b.op == TokenKind::EqEq || b.op == TokenKind::BangEq ||
        b.op == TokenKind::Lt || b.op == TokenKind::Gt ||
        b.op == TokenKind::LtEq || b.op == TokenKind::GtEq ||
        b.op == TokenKind::AmpAmp || b.op == TokenKind::PipePipe)
      return SemaType::boolTy();
    if (b.op == TokenKind::Plus && lty && lty->isString() && rty && rty->isString())
      return lty;
    return lty ? lty : rty;
  }
  case NodeKind::UnaryExpr: {
    auto &u = static_cast<UnaryExpr &>(n);
    auto ty = analyzeExpr(*u.operand);
    if (u.op == TokenKind::Bang)
      return SemaType::boolTy();
    if (u.op == TokenKind::Star)
      return ty && ty->inner ? ty->inner : SemaType::voidTy();
    if (u.op == TokenKind::Amp)
      return SemaType::refTy(ty);
    return ty;
  }
  case NodeKind::CallExpr: {
    auto &c = static_cast<CallExpr &>(n);
    auto calleeTy = analyzeExpr(*c.callee);
    for (auto &a : c.args)
      analyzeExpr(*a);
    if (calleeTy && calleeTy->kind == TypeKind::Fn && calleeTy->ret) {
      return calleeTy->ret;
    }
    return SemaType::voidTy();
  }
  case NodeKind::FieldExpr: {
    auto &f = static_cast<FieldExpr &>(n);
    auto objTy = analyzeExpr(*f.obj);
    if (objTy && objTy->kind == TypeKind::Struct) {
      for (size_t i = 0; i < objTy->fieldNames.size(); ++i) {
        if (objTy->fieldNames[i] == f.field) {
          if (i < objTy->params.size())
            return objTy->params[i];
          break;
        }
      }
    }
    return SemaType::voidTy();
  }
  case NodeKind::StructExpr: {
    auto &s = static_cast<StructExpr &>(n);
    if (!s.path.empty()) {
      auto sym = lookupSymbol(s.path.back());
      if (sym && sym->type)
        return sym->type;
    }
    for (auto &f : s.fields)
      if (f)
        analyzeExpr(*f->value);
    return SemaType::voidTy();
  }
  case NodeKind::IndexExpr: {
    auto &i = static_cast<IndexExpr &>(n);
    analyzeExpr(*i.obj);
    analyzeExpr(*i.idx);
    return SemaType::voidTy();
  }
  case NodeKind::CastExpr: {
    auto &c = static_cast<CastExpr &>(n);
    analyzeExpr(*c.obj);
    return c.ty ? resolveTypeNode(*c.ty) : SemaType::voidTy();
  }
  case NodeKind::AssignExpr: {
    auto &a = static_cast<AssignExpr &>(n);
    analyzeExpr(*a.lhs);
    return analyzeExpr(*a.rhs);
  }
  case NodeKind::LambdaExpr: {
    auto &lambda = static_cast<LambdaExpr &>(n);
    auto fnTy = std::make_shared<SemaType>();
    fnTy->kind = TypeKind::Fn;
    for (auto &p : lambda.params) {
      auto pTy = p->ty ? resolveTypeNode(*p->ty) : SemaType::i64Ty();
      fnTy->params.push_back(pTy);
    }
    fnTy->ret = lambda.body ? analyzeExpr(*lambda.body) : SemaType::i64Ty();
    return fnTy;
  }
  case NodeKind::ExtensionNode: {
    auto &ext = static_cast<ExtensionNode &>(n);
    for (auto &ep : m_extensions) {
      if (ep.extensionId == ext.extensionId)
        return ep.handler(ext, *this);
    }
    for (auto &c : ext.children)
      if (c)
        analyzeExpr(*c);
    return SemaType::voidTy();
  }
  default:
    return SemaType::voidTy();
  }
}

TypeRef Sema::inferExpr(Node &n) { return analyzeExpr(n); }

} // namespace lumora
