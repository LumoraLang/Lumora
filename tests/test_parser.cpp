#include "lumora/Lexer.h"
#include "lumora/Parser.h"
#include <cassert>
#include <iostream>

using namespace lumora;
using namespace lumora::ast;

static std::unique_ptr<Module> parse(std::string_view src) {
    Lexer  lex(src, "<test>");
    Parser p(lex);
    return p.parseModule("<test>");
}

static void testFnDecl() {
    auto mod = parse("fn add(a: i64, b: i64) -> i64 { return a + b; }");
    assert(!mod->items.empty());
    assert(mod->items[0]->kind == NodeKind::FnDecl);

    auto& fn = static_cast<FnDecl&>(*mod->items[0]);
    assert(fn.name == "add");
    assert(fn.params.size() == 2);
    assert(fn.params[0]->name == "a");
    assert(fn.params[1]->name == "b");
    assert(fn.body != nullptr);

    std::cout << "testFnDecl: PASS\n";
}

static void testStructDecl() {
    auto mod = parse("struct Point { x: f64, y: f64, }");
    assert(!mod->items.empty());
    assert(mod->items[0]->kind == NodeKind::StructDecl);

    auto& s = static_cast<StructDecl&>(*mod->items[0]);
    assert(s.name == "Point");
    assert(s.fields.size() == 2);
    assert(s.fields[0]->name == "x");
    assert(s.fields[1]->name == "y");

    std::cout << "testStructDecl: PASS\n";
}

static void testEnumDecl() {
    auto mod = parse("enum Dir { North, South, East, West }");
    assert(!mod->items.empty());
    assert(mod->items[0]->kind == NodeKind::EnumDecl);

    auto& e = static_cast<EnumDecl&>(*mod->items[0]);
    assert(e.name == "Dir");
    assert(e.variants.size() == 4);

    std::cout << "testEnumDecl: PASS\n";
}

static void testIfWhileFor() {
    auto mod = parse(R"(
        fn test() {
            let mut x = 0;
            if x == 0 { x = 1; }
            while x < 10 { x = x + 1; }
            for i in 10 { x = x + i; }
        }
    )");
    assert(!mod->items.empty());
    auto& fn = static_cast<FnDecl&>(*mod->items[0]);
    auto& body = static_cast<BlockStmt&>(*fn.body);
    assert(body.stmts.size() == 4);
    assert(body.stmts[0]->kind == NodeKind::LetStmt);
    assert(body.stmts[1]->kind == NodeKind::IfStmt);
    assert(body.stmts[2]->kind == NodeKind::WhileStmt);
    assert(body.stmts[3]->kind == NodeKind::ForStmt);

    std::cout << "testIfWhileFor: PASS\n";
}

static void testBinaryPrecedence() {
    auto mod = parse("fn f() -> i64 { return 2 + 3 * 4; }");
    auto& fn   = static_cast<FnDecl&>(*mod->items[0]);
    auto& body = static_cast<BlockStmt&>(*fn.body);
    auto& ret  = static_cast<ReturnStmt&>(*body.stmts[0]);
    assert(ret.value->kind == NodeKind::BinaryExpr);
    auto& add = static_cast<BinaryExpr&>(*ret.value);
    assert(add.op == TokenKind::Plus);
    assert(add.rhs->kind == NodeKind::BinaryExpr);
    auto& mul = static_cast<BinaryExpr&>(*add.rhs);
    assert(mul.op == TokenKind::Star);

    std::cout << "testBinaryPrecedence: PASS\n";
}

static void testLambda() {
    auto mod = parse("fn f() { let add = |a, b| a + b; }");
    assert(!mod->items.empty());

    std::cout << "testLambda: PASS\n";
}

static void testMatchExpr() {
    auto mod = parse(R"(
        fn classify(n: i64) -> i64 {
            match n {
                0 => return 0,
                1 | 2 => return 1,
                _ => return -1,
            }
            return 0;
        }
    )");
    assert(!mod->items.empty());
    std::cout << "testMatchExpr: PASS\n";
}

int main() {
    testFnDecl();
    testStructDecl();
    testEnumDecl();
    testIfWhileFor();
    testBinaryPrecedence();
    testLambda();
    testMatchExpr();
    std::cout << "All parser tests passed!\n";
    return 0;
}
