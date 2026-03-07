#include "lumora/Parser.h"
#include <format>
#include <stdexcept>
#include <regex>

namespace lumora {

using namespace ast;

Parser::Parser(Lexer& lex) : m_lex(lex) {}

void Parser::registerExtension(ParserExtensionPoint ext) {
    m_extensions.push_back(std::move(ext));
}

const std::vector<ParseError>& Parser::errors() const noexcept { return m_errors; }
bool Parser::hasErrors() const noexcept { return !m_errors.empty(); }

Token Parser::peek(size_t n) { return m_lex.peek(n); }
Token Parser::eat()          { return m_lex.next(); }

bool Parser::check(TokenKind k) { return peek().is(k); }

bool Parser::match(TokenKind k) {
    if (check(k)) { eat(); return true; }
    return false;
}

Token Parser::expect(TokenKind k, std::string_view msg) {
    if (!check(k)) {
        auto t = peek();
        auto err = msg.empty()
            ? std::format("expected '{}', got '{}'", tokenKindName(k), t.raw)
            : std::string(msg);
        error(err, t.loc);
        return t;
    }
    return eat();
}

void Parser::error(std::string_view msg, SourceLoc loc) {
    m_errors.push_back({std::string(msg), loc});
}

void Parser::synchronize() {
    while (!check(TokenKind::Eof)) {
        auto k = peek().kind;
        if (k == TokenKind::Semicolon) { eat(); return; }
        if (k == TokenKind::KwFn  || k == TokenKind::KwStruct ||
            k == TokenKind::KwEnum || k == TokenKind::KwImpl  ||
            k == TokenKind::KwLet  || k == TokenKind::KwReturn) return;
        eat();
    }
}

std::unique_ptr<Module> Parser::parseModule(std::string_view filename) {
    auto mod = std::make_unique<Module>();
    mod->file = std::string(filename);
    mod->loc  = peek().loc;

    while (!check(TokenKind::Eof)) {
        try {
            auto item = parseTopLevelItem();
            if (item) mod->items.push_back(std::move(item));
        } catch (const std::exception& e) {
            error(e.what(), peek().loc);
            synchronize();
        }
    }
    return mod;
}

std::vector<Attribute> Parser::parseAttributes() {
    std::vector<Attribute> attrs;
    while (check(TokenKind::At)) {
        eat();
        Attribute a;
        a.loc  = peek().loc;
        a.name = expect(TokenKind::Ident).raw;
        if (match(TokenKind::LParen)) {
            while (!check(TokenKind::RParen) && !check(TokenKind::Eof)) {
                a.args.push_back(eat());
            }
            expect(TokenKind::RParen);
        }
        attrs.push_back(std::move(a));
    }
    return attrs;
}

NodePtr Parser::tryExtension() {
    if (check(TokenKind::At)) {
        auto nextTok = peek(1);
        for (auto& ext : m_extensions) {
            std::regex re(ext.triggerPattern);
            if (std::regex_match(nextTok.raw, re)) {
                eat(); // @
                auto trigger = eat();
                return ext.handler(*this, trigger);
            }
        }
    }

    for (auto& ext : m_extensions) {
        std::regex re(ext.triggerPattern);
        auto tok = peek();
        if (std::regex_match(tok.raw, re)) {
            auto trigger = eat();
            return ext.handler(*this, trigger);
        }
    }
    return nullptr;
}

NodePtr Parser::parseTopLevelItem() {
    auto attrs = parseAttributes();

    if (auto extNode = tryExtension()) return extNode;

    bool isPub = match(TokenKind::KwPub);

    switch (peek().kind) {
        case TokenKind::KwFn:     return parseFnDecl(isPub);
        case TokenKind::KwStruct: return parseStructDecl(isPub);
        case TokenKind::KwEnum:   return parseEnumDecl(isPub);
        case TokenKind::KwImpl:   return parseImplDecl();
        case TokenKind::KwTrait:  return parseTraitDecl(isPub);
        case TokenKind::KwType:   return parseTypeAlias(isPub);
        case TokenKind::KwUse:    return parseUseDecl(isPub);
        case TokenKind::KwMod:    return parseModDecl(isPub);
        case TokenKind::KwExtern: return parseExternDecl();
        case TokenKind::KwConst:  return parseLetStmt();
        default:
            error(std::format("unexpected token '{}' at top level", peek().raw), peek().loc);
            eat();
            return nullptr;
    }
}

std::unique_ptr<FnDecl> Parser::parseFnDecl(bool isPub) {
    auto fn  = std::make_unique<FnDecl>();
    fn->loc  = peek().loc;
    fn->isPub = isPub;
    expect(TokenKind::KwFn);
    fn->name = expect(TokenKind::Ident).raw;

    if (match(TokenKind::Lt)) {
        while (!check(TokenKind::Gt) && !check(TokenKind::Eof)) {
            fn->generics.push_back(expect(TokenKind::Ident).raw);
            if (!match(TokenKind::Comma)) break;
        }
        expect(TokenKind::Gt);
    }

    expect(TokenKind::LParen);
    while (!check(TokenKind::RParen) && !check(TokenKind::Eof)) {
        auto p    = std::make_unique<ParamDecl>();
        p->loc    = peek().loc;
        p->isMut  = match(TokenKind::KwMut);
        if (check(TokenKind::Ellipsis)) {
            eat();
            p->isVararg = true;
            fn->params.push_back(std::move(p));
            break;
        }
        p->name = expect(TokenKind::Ident).raw;
        expect(TokenKind::Colon);
        p->ty = parseType();
        if (match(TokenKind::Eq)) p->defaultVal = parseExpr();
        fn->params.push_back(std::move(p));
        if (!match(TokenKind::Comma)) break;
    }
    expect(TokenKind::RParen);

    if (match(TokenKind::Arrow)) fn->retTy = parseType();

    if (check(TokenKind::LBrace)) fn->body = parseBlock();
    else                          expect(TokenKind::Semicolon);

    return fn;
}

std::unique_ptr<StructDecl> Parser::parseStructDecl(bool isPub) {
    auto s  = std::make_unique<StructDecl>();
    s->loc  = peek().loc;
    s->isPub = isPub;
    expect(TokenKind::KwStruct);
    s->name = expect(TokenKind::Ident).raw;

    expect(TokenKind::LBrace);
    while (!check(TokenKind::RBrace) && !check(TokenKind::Eof)) {
        auto f    = std::make_unique<FieldDecl>();
        f->loc    = peek().loc;
        f->isPub  = match(TokenKind::KwPub);
        f->name   = expect(TokenKind::Ident).raw;
        expect(TokenKind::Colon);
        f->ty = parseType();
        if (match(TokenKind::Eq)) f->defaultVal = parseExpr();
        expect(TokenKind::Comma);
        s->fields.push_back(std::move(f));
    }
    expect(TokenKind::RBrace);
    return s;
}

std::unique_ptr<EnumDecl> Parser::parseEnumDecl(bool isPub) {
    auto e  = std::make_unique<EnumDecl>();
    e->loc  = peek().loc;
    e->isPub = isPub;
    expect(TokenKind::KwEnum);
    e->name = expect(TokenKind::Ident).raw;
    expect(TokenKind::LBrace);
    while (!check(TokenKind::RBrace) && !check(TokenKind::Eof)) {
        auto v = std::make_unique<EnumVariant>();
        v->loc  = peek().loc;
        v->name = expect(TokenKind::Ident).raw;
        if (match(TokenKind::LParen)) {
            while (!check(TokenKind::RParen) && !check(TokenKind::Eof)) {
                v->fields.push_back(parseType());
                if (!match(TokenKind::Comma)) break;
            }
            expect(TokenKind::RParen);
        }
        if (match(TokenKind::Eq)) v->discriminant = parseExpr();
        e->variants.push_back(std::move(v));
        if (!match(TokenKind::Comma)) break;
    }
    expect(TokenKind::RBrace);
    return e;
}

std::unique_ptr<ImplDecl> Parser::parseImplDecl() {
    auto impl = std::make_unique<ImplDecl>();
    impl->loc = peek().loc;
    expect(TokenKind::KwImpl);
    impl->ty = parseType();
    if (match(TokenKind::KwFor)) {
        auto trt  = parseType();
        impl->traitTy = std::move(trt);
    }
    expect(TokenKind::LBrace);
    while (!check(TokenKind::RBrace) && !check(TokenKind::Eof)) {
        bool pub = match(TokenKind::KwPub);
        impl->items.push_back(parseFnDecl(pub));
    }
    expect(TokenKind::RBrace);
    return impl;
}

std::unique_ptr<TraitDecl> Parser::parseTraitDecl(bool isPub) {
    auto t  = std::make_unique<TraitDecl>();
    t->loc  = peek().loc;
    t->isPub = isPub;
    expect(TokenKind::KwTrait);
    t->name = expect(TokenKind::Ident).raw;
    expect(TokenKind::LBrace);
    while (!check(TokenKind::RBrace) && !check(TokenKind::Eof)) {
        bool pub = match(TokenKind::KwPub);
        t->items.push_back(parseFnDecl(pub));
    }
    expect(TokenKind::RBrace);
    return t;
}

std::unique_ptr<TypeAlias> Parser::parseTypeAlias(bool isPub) {
    auto a  = std::make_unique<TypeAlias>();
    a->loc  = peek().loc;
    a->isPub = isPub;
    expect(TokenKind::KwType);
    a->name = expect(TokenKind::Ident).raw;
    expect(TokenKind::Eq);
    a->ty = parseType();
    expect(TokenKind::Semicolon);
    return a;
}

std::unique_ptr<UseDecl> Parser::parseUseDecl(bool isPub) {
    auto u  = std::make_unique<UseDecl>();
    u->loc  = peek().loc;
    u->isPub = isPub;
    expect(TokenKind::KwUse);
    u->path.push_back(expect(TokenKind::Ident).raw);
    while (match(TokenKind::DoubleColon)) u->path.push_back(expect(TokenKind::Ident).raw);
    if (match(TokenKind::KwAs)) u->alias = expect(TokenKind::Ident).raw;
    expect(TokenKind::Semicolon);
    return u;
}

std::unique_ptr<ModDecl> Parser::parseModDecl(bool isPub) {
    auto m  = std::make_unique<ModDecl>();
    m->loc  = peek().loc;
    m->isPub = isPub;
    expect(TokenKind::KwMod);
    m->name = expect(TokenKind::Ident).raw;
    if (check(TokenKind::LBrace)) {
        eat();
        while (!check(TokenKind::RBrace) && !check(TokenKind::Eof))
            m->items.push_back(parseTopLevelItem());
        expect(TokenKind::RBrace);
    } else {
        expect(TokenKind::Semicolon);
    }
    return m;
}

std::unique_ptr<ExternDecl> Parser::parseExternDecl() {
    auto e = std::make_unique<ExternDecl>();
    e->loc = peek().loc;
    expect(TokenKind::KwExtern);
    e->abi = "C";
    if (check(TokenKind::LitString)) {
        auto tok = eat();
        e->abi = std::get<std::string>(tok.extra);
    }
    expect(TokenKind::LBrace);
    while (!check(TokenKind::RBrace) && !check(TokenKind::Eof)) {
        bool pub = match(TokenKind::KwPub);
        e->items.push_back(parseFnDecl(pub));
    }
    expect(TokenKind::RBrace);
    return e;
}

std::unique_ptr<BlockStmt> Parser::parseBlock() {
    auto b = std::make_unique<BlockStmt>();
    b->loc = peek().loc;
    expect(TokenKind::LBrace);
    while (!check(TokenKind::RBrace) && !check(TokenKind::Eof))
        b->stmts.push_back(parseStmt());
    expect(TokenKind::RBrace);
    return b;
}

NodePtr Parser::parseStmt() {
    if (auto extNode = tryExtension()) return extNode;

    switch (peek().kind) {
        case TokenKind::KwLet:      return parseLetStmt();
        case TokenKind::KwConst:    return parseLetStmt();
        case TokenKind::KwReturn:   return parseReturnStmt();
        case TokenKind::KwIf:       return parseIfStmt();
        case TokenKind::KwWhile:    return parseWhileStmt();
        case TokenKind::KwFor:      return parseForStmt();
        case TokenKind::KwMatch:    return parseMatchExpr();
        case TokenKind::KwDefer:    return parseDeferStmt();
        case TokenKind::KwBreak: {
            auto s = std::make_unique<BreakStmt>(); s->loc = eat().loc;
            if (check(TokenKind::Ident)) s->label = eat().raw;
            match(TokenKind::Semicolon);
            return s;
        }
        case TokenKind::KwContinue: {
            auto s = std::make_unique<ContinueStmt>(); s->loc = eat().loc;
            if (check(TokenKind::Ident)) s->label = eat().raw;
            match(TokenKind::Semicolon);
            return s;
        }
        case TokenKind::LBrace: {
            return parseBlock();
        }
        default: {
            auto e  = std::make_unique<ExprStmt>();
            e->loc  = peek().loc;
            e->expr = parseExpr();
            match(TokenKind::Semicolon);
            return e;
        }
    }
}

NodePtr Parser::parseLetStmt() {
    auto l   = std::make_unique<LetStmt>();
    l->loc   = peek().loc;
    bool isConst = check(TokenKind::KwConst);
    eat();
    l->isMut = !isConst && match(TokenKind::KwMut);
    l->name  = expect(TokenKind::Ident).raw;
    if (match(TokenKind::Colon)) l->ty = parseType();
    if (match(TokenKind::Eq))    l->init = parseExpr();
    expect(TokenKind::Semicolon);
    return l;
}

NodePtr Parser::parseReturnStmt() {
    auto r = std::make_unique<ReturnStmt>();
    r->loc  = eat().loc;
    if (!check(TokenKind::Semicolon) && !check(TokenKind::RBrace))
        r->value = parseExpr();
    match(TokenKind::Semicolon);
    return r;
}

NodePtr Parser::parseIfStmt() {
    auto s = std::make_unique<IfStmt>();
    s->loc = eat().loc;
    s->cond       = parseExpr();
    s->thenBranch = parseBlock();
    if (match(TokenKind::KwElse)) {
        if (check(TokenKind::KwIf)) s->elseBranch = parseIfStmt();
        else                        s->elseBranch  = parseBlock();
    }
    return s;
}

NodePtr Parser::parseWhileStmt() {
    auto s = std::make_unique<WhileStmt>();
    s->loc = eat().loc;
    s->cond = parseExpr();
    s->body = parseBlock();
    return s;
}

NodePtr Parser::parseForStmt() {
    auto s = std::make_unique<ForStmt>();
    s->loc = eat().loc;
    s->var = expect(TokenKind::Ident).raw;
    expect(TokenKind::KwIn);
    s->iter = parseExpr();
    s->body = parseBlock();
    return s;
}

NodePtr Parser::parseDeferStmt() {
    auto d = std::make_unique<DeferStmt>();
    d->loc  = eat().loc;
    d->expr = parseExpr();
    match(TokenKind::Semicolon);
    return d;
}

NodePtr Parser::parseMatchExpr() {
    auto m = std::make_unique<MatchExpr>();
    m->loc = eat().loc;
    m->subject = parseExpr();
    expect(TokenKind::LBrace);
    while (!check(TokenKind::RBrace) && !check(TokenKind::Eof)) {
        auto arm = std::make_unique<MatchArm>();
        arm->loc = peek().loc;
        arm->patterns.push_back(parseExpr());
        while (match(TokenKind::Pipe)) arm->patterns.push_back(parseExpr());
        if (match(TokenKind::KwIf)) arm->guard = parseExpr();
        expect(TokenKind::FatArrow);
        arm->body = check(TokenKind::LBrace) ? parseBlock() : parseExpr();
        match(TokenKind::Comma);
        m->arms.push_back(std::move(arm));
    }
    expect(TokenKind::RBrace);
    return m;
}

int Parser::binOpPrec(TokenKind k) const noexcept {
    switch (k) {
        case TokenKind::PipePipe:  return 1;
        case TokenKind::AmpAmp:    return 2;
        case TokenKind::Pipe:      return 3;
        case TokenKind::Caret:     return 4;
        case TokenKind::Amp:       return 5;
        case TokenKind::EqEq:
        case TokenKind::BangEq:    return 6;
        case TokenKind::Lt:
        case TokenKind::LtEq:
        case TokenKind::Gt:
        case TokenKind::GtEq:      return 7;
        case TokenKind::LtLt:
        case TokenKind::GtGt:      return 8;
        case TokenKind::Plus:
        case TokenKind::Minus:     return 9;
        case TokenKind::Star:
        case TokenKind::Slash:
        case TokenKind::Percent:   return 10;
        case TokenKind::StarStar:  return 11;
        default:                   return -1;
    }
}

bool Parser::isAssignOp(TokenKind k) const noexcept {
    switch (k) {
        case TokenKind::Eq: case TokenKind::PlusEq: case TokenKind::MinusEq:
        case TokenKind::StarEq: case TokenKind::SlashEq: case TokenKind::PercentEq:
        case TokenKind::AmpEq: case TokenKind::PipeEq: case TokenKind::CaretEq:
        case TokenKind::LtLtEq: case TokenKind::GtGtEq:
            return true;
        default: return false;
    }
}

NodePtr Parser::parseExpr(int minPrec) {
    auto lhs = parseUnaryExpr();

    if (isAssignOp(peek().kind)) {
        auto op  = eat();
        auto rhs = parseExpr(0);
        auto a   = std::make_unique<AssignExpr>();
        a->loc   = op.loc;
        a->op    = op.kind;
        a->lhs   = std::move(lhs);
        a->rhs   = std::move(rhs);
        return a;
    }

    if (match(TokenKind::KwAs)) {
        auto cast = std::make_unique<CastExpr>();
        cast->loc = peek().loc;
        cast->obj = std::move(lhs);
        cast->ty  = parseType();
        lhs       = std::move(cast);
    }

    while (true) {
        int prec = binOpPrec(peek().kind);
        if (prec < minPrec || prec == -1) break;
        auto op  = eat();
        auto rhs = parseExpr(prec + 1);
        auto bin = std::make_unique<BinaryExpr>();
        bin->loc = op.loc;
        bin->op  = op.kind;
        bin->lhs = std::move(lhs);
        bin->rhs = std::move(rhs);
        lhs      = std::move(bin);
    }

    return lhs;
}

NodePtr Parser::parseUnaryExpr() {
    switch (peek().kind) {
        case TokenKind::Minus:
        case TokenKind::Bang:
        case TokenKind::Tilde:
        case TokenKind::Star:
        case TokenKind::Amp: {
            auto tok = eat();
            auto u   = std::make_unique<UnaryExpr>();
            u->loc     = tok.loc;
            u->op      = tok.kind;
            u->operand = parseUnaryExpr();
            return u;
        }
        default:
            return parsePostfixExpr(parsePrimaryExpr());
    }
}

NodePtr Parser::parsePostfixExpr(NodePtr base) {
    while (true) {
        if (check(TokenKind::LParen)) {
            auto c  = std::make_unique<CallExpr>();
            c->loc  = eat().loc;
            c->callee = std::move(base);
            while (!check(TokenKind::RParen) && !check(TokenKind::Eof)) {
                c->args.push_back(parseExpr());
                if (!match(TokenKind::Comma)) break;
            }
            expect(TokenKind::RParen);
            base = std::move(c);
        } else if (check(TokenKind::LBracket)) {
            auto i = std::make_unique<IndexExpr>();
            i->loc = eat().loc;
            i->obj = std::move(base);
            i->idx = parseExpr();
            expect(TokenKind::RBracket);
            base = std::move(i);
        } else if (match(TokenKind::Dot)) {
            auto f = std::make_unique<FieldExpr>();
            f->loc   = peek().loc;
            f->field = expect(TokenKind::Ident).raw;
            f->obj   = std::move(base);
            base     = std::move(f);
        } else if (check(TokenKind::PlusPlus) || check(TokenKind::MinusMinus)) {
            auto tok = eat();
            auto u   = std::make_unique<UnaryExpr>();
            u->loc      = tok.loc;
            u->op       = tok.kind;
            u->operand  = std::move(base);
            u->isPostfix = true;
            base         = std::move(u);
        } else {
            break;
        }
    }
    return base;
}

NodePtr Parser::parsePrimaryExpr() {
    if (auto ext = tryExtension()) return ext;

    switch (peek().kind) {
        case TokenKind::LitInt: {
            auto t = eat();
            auto n = std::make_unique<IntLit>();
            n->loc   = t.loc;
            n->value = std::get<int64_t>(t.extra);
            return n;
        }
        case TokenKind::LitFloat: {
            auto t = eat();
            auto n = std::make_unique<FloatLit>();
            n->loc   = t.loc;
            n->value = std::get<double>(t.extra);
            return n;
        }
        case TokenKind::LitString: {
            auto t = eat();
            auto n = std::make_unique<StringLit>();
            n->loc   = t.loc;
            n->value = std::get<std::string>(t.extra);
            return n;
        }
        case TokenKind::LitChar: {
            auto t = eat();
            auto n = std::make_unique<CharLit>();
            n->loc   = t.loc;
            n->value = static_cast<char>(std::get<int64_t>(t.extra));
            return n;
        }
        case TokenKind::KwTrue:
        case TokenKind::KwFalse: {
            auto t = eat();
            auto n = std::make_unique<BoolLit>();
            n->loc   = t.loc;
            n->value = t.is(TokenKind::KwTrue);
            return n;
        }
        case TokenKind::KwNull: {
            auto t = eat();
            auto n = std::make_unique<NullLit>();
            n->loc  = t.loc;
            return n;
        }
        case TokenKind::KwSizeof: {
            auto t  = eat(); expect(TokenKind::LParen);
            auto n  = std::make_unique<SizeofExpr>();
            n->loc  = t.loc;
            n->ty   = parseType();
            expect(TokenKind::RParen);
            return n;
        }
        case TokenKind::KwAlignof: {
            auto t = eat(); expect(TokenKind::LParen);
            auto n = std::make_unique<AlignofExpr>();
            n->loc = t.loc;
            n->ty  = parseType();
            expect(TokenKind::RParen);
            return n;
        }
        case TokenKind::KwTypeof: {
            auto t = eat(); expect(TokenKind::LParen);
            auto n = std::make_unique<TypeofExpr>();
            n->loc  = t.loc;
            n->expr = parseExpr();
            expect(TokenKind::RParen);
            return n;
        }
        case TokenKind::KwOffsetof: {
            auto t = eat(); expect(TokenKind::LParen);
            auto n  = std::make_unique<OffsetofExpr>();
            n->loc  = t.loc;
            n->ty   = parseType();
            expect(TokenKind::Comma);
            n->field = expect(TokenKind::Ident).raw;
            expect(TokenKind::RParen);
            return n;
        }
        case TokenKind::LBracket: {
            auto a = std::make_unique<ArrayExpr>();
            a->loc = eat().loc;
            while (!check(TokenKind::RBracket) && !check(TokenKind::Eof)) {
                a->elems.push_back(parseExpr());
                if (!match(TokenKind::Comma)) break;
            }
            expect(TokenKind::RBracket);
            return a;
        }
        case TokenKind::LParen: {
            eat();
            auto e = parseExpr();
            expect(TokenKind::RParen);
            return e;
        }
        case TokenKind::Pipe:
        case TokenKind::PipePipe: {
            auto l   = std::make_unique<LambdaExpr>();
            l->loc   = peek().loc;
            bool double_pipe = check(TokenKind::PipePipe);
            eat();
            if (!double_pipe) {
                while (!check(TokenKind::Pipe) && !check(TokenKind::Eof)) {
                    auto p   = std::make_unique<ParamDecl>();
                    p->loc   = peek().loc;
                    p->name  = expect(TokenKind::Ident).raw;
                    if (match(TokenKind::Colon)) p->ty = parseType();
                    l->params.push_back(std::move(p));
                    if (!match(TokenKind::Comma)) break;
                }
                expect(TokenKind::Pipe);
            }
            if (match(TokenKind::Arrow)) l->retTy = parseType();
            l->body = check(TokenKind::LBrace) ? parseBlock() : parseExpr();
            return l;
        }
        case TokenKind::Ident: {
            auto path = std::make_unique<PathExpr>();
            path->loc = peek().loc;
            path->segments.push_back(eat().raw);
            while (match(TokenKind::DoubleColon)) {
                path->segments.push_back(expect(TokenKind::Ident).raw);
            }
            if (path->segments.size() == 1) {
                auto id  = std::make_unique<IdentExpr>();
                id->loc  = path->loc;
                id->name = path->segments[0];
                return id;
            }
            if (check(TokenKind::LBrace)) {
                auto s = std::make_unique<StructExpr>();
                s->loc  = path->loc;
                s->path = std::move(path->segments);
                eat();
                while (!check(TokenKind::RBrace) && !check(TokenKind::Eof)) {
                    auto f = std::make_unique<StructExprField>();
                    f->loc  = peek().loc;
                    f->name = expect(TokenKind::Ident).raw;
                    expect(TokenKind::Colon);
                    f->value = parseExpr();
                    match(TokenKind::Comma);
                    s->fields.push_back(std::move(f));
                }
                expect(TokenKind::RBrace);
                return s;
            }
            return path;
        }
        default: {
            error(std::format("unexpected token '{}' in expression", peek().raw), peek().loc);
            eat();
            auto n = std::make_unique<NullLit>();
            return n;
        }
    }
}

TypePtr Parser::parseType() {
    switch (peek().kind) {
        case TokenKind::Star:    return parsePtrOrRefType();
        case TokenKind::Amp:     return parsePtrOrRefType();
        case TokenKind::LBracket: return parseArrayOrSliceType();
        case TokenKind::LParen:   return parseTupleType();
        case TokenKind::KwFn:     return parseFnType();
        default:
            break;
    }

    static const std::unordered_map<std::string_view, PrimTypeNode::Prim> prims = {
        {"i8",    PrimTypeNode::Prim::I8},   {"i16", PrimTypeNode::Prim::I16},
        {"i32",   PrimTypeNode::Prim::I32},  {"i64", PrimTypeNode::Prim::I64},
        {"i128",  PrimTypeNode::Prim::I128}, {"u8",  PrimTypeNode::Prim::U8},
        {"u16",   PrimTypeNode::Prim::U16},  {"u32", PrimTypeNode::Prim::U32},
        {"u64",   PrimTypeNode::Prim::U64},  {"u128",PrimTypeNode::Prim::U128},
        {"f32",   PrimTypeNode::Prim::F32},  {"f64", PrimTypeNode::Prim::F64},
        {"bool",  PrimTypeNode::Prim::Bool}, {"char",PrimTypeNode::Prim::Char},
        {"void",  PrimTypeNode::Prim::Void}, {"never",PrimTypeNode::Prim::Never},
    };

    if (check(TokenKind::Ident)) {
        auto tok = peek();
        auto it  = prims.find(tok.raw);
        if (it != prims.end()) {
            eat();
            auto n  = std::make_unique<PrimTypeNode>();
            n->loc  = tok.loc;
            n->prim = it->second;
            return n;
        }
        auto n = std::make_unique<NamedTypeNode>();
        n->loc = tok.loc;
        n->path.push_back(eat().raw);
        while (match(TokenKind::DoubleColon)) n->path.push_back(expect(TokenKind::Ident).raw);
        if (match(TokenKind::Lt)) {
            while (!check(TokenKind::Gt) && !check(TokenKind::Eof)) {
                n->typeArgs.push_back(parseType());
                if (!match(TokenKind::Comma)) break;
            }
            expect(TokenKind::Gt);
        }
        return n;
    }

    error(std::format("expected type, got '{}'", peek().raw), peek().loc);
    auto n = std::make_unique<PrimTypeNode>();
    n->prim = PrimTypeNode::Prim::Void;
    return n;
}

TypePtr Parser::parsePtrOrRefType() {
    auto isRef = check(TokenKind::Amp);
    auto loc   = eat().loc;
    bool isMut = match(TokenKind::KwMut);
    auto inner = parseType();
    if (isRef) {
        auto r   = std::make_unique<RefTypeNode>();
        r->loc   = loc;
        r->isMut = isMut;
        r->inner = std::move(inner);
        return r;
    }
    auto p   = std::make_unique<PtrTypeNode>();
    p->loc   = loc;
    p->isMut = isMut;
    p->inner = std::move(inner);
    return p;
}

TypePtr Parser::parseArrayOrSliceType() {
    auto loc = eat().loc;
    auto inner = parseType();
    if (match(TokenKind::Semicolon)) {
        auto a   = std::make_unique<ArrayTypeNode>();
        a->loc   = loc;
        a->inner = std::move(inner);
        a->size  = parseExpr();
        expect(TokenKind::RBracket);
        return a;
    }
    expect(TokenKind::RBracket);
    auto s   = std::make_unique<SliceTypeNode>();
    s->loc   = loc;
    s->inner = std::move(inner);
    return s;
}

TypePtr Parser::parseFnType() {
    auto loc = eat().loc;
    auto f   = std::make_unique<FnTypeNode>();
    f->loc   = loc;
    expect(TokenKind::LParen);
    while (!check(TokenKind::RParen) && !check(TokenKind::Eof)) {
        if (check(TokenKind::Ellipsis)) { eat(); f->isVararg = true; break; }
        f->params.push_back(parseType());
        if (!match(TokenKind::Comma)) break;
    }
    expect(TokenKind::RParen);
    if (match(TokenKind::Arrow)) f->ret = parseType();
    return f;
}

TypePtr Parser::parseTupleType() {
    auto loc = eat().loc;
    auto t   = std::make_unique<TupleTypeNode>();
    t->loc   = loc;
    while (!check(TokenKind::RParen) && !check(TokenKind::Eof)) {
        t->elems.push_back(parseType());
        if (!match(TokenKind::Comma)) break;
    }
    expect(TokenKind::RParen);
    return t;
}

} 
