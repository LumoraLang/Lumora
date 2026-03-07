#include "lumora/Lexer.h"
#include <cassert>
#include <iostream>

using namespace lumora;

static void testBasicTokens() {
    Lexer lex("fn main() -> i32 { return 42; }", "<test>");
    auto toks = lex.tokenizeAll();

    assert(toks[0].is(TokenKind::KwFn));
    assert(toks[1].is(TokenKind::Ident) && toks[1].raw == "main");
    assert(toks[2].is(TokenKind::LParen));
    assert(toks[3].is(TokenKind::RParen));
    assert(toks[4].is(TokenKind::Arrow));
    assert(toks[5].is(TokenKind::Ident) && toks[5].raw == "i32");
    assert(toks[6].is(TokenKind::LBrace));
    assert(toks[7].is(TokenKind::KwReturn));
    assert(toks[8].is(TokenKind::LitInt));
    assert(std::get<int64_t>(toks[8].extra) == 42);
    assert(toks[9].is(TokenKind::Semicolon));
    assert(toks[10].is(TokenKind::RBrace));
    assert(toks[11].is(TokenKind::Eof));

    std::cout << "testBasicTokens: PASS\n";
}

static void testNumberLiterals() {
    Lexer lex("42 3.14 0xFF 0b1010 1_000_000", "<test>");
    auto toks = lex.tokenizeAll();

    assert(toks[0].is(TokenKind::LitInt)   && std::get<int64_t>(toks[0].extra) == 42);
    assert(toks[1].is(TokenKind::LitFloat) && std::get<double>(toks[1].extra) == 3.14);
    assert(toks[2].is(TokenKind::LitInt)   && std::get<int64_t>(toks[2].extra) == 0xFF);
    assert(toks[3].is(TokenKind::LitInt)   && std::get<int64_t>(toks[3].extra) == 0b1010);
    assert(toks[4].is(TokenKind::LitInt)   && std::get<int64_t>(toks[4].extra) == 1000000);

    std::cout << "testNumberLiterals: PASS\n";
}

static void testStringLiteral() {
    Lexer lex("\"hello\\nworld\"", "<test>");
    auto toks = lex.tokenizeAll();

    assert(toks[0].is(TokenKind::LitString));
    assert(std::get<std::string>(toks[0].extra) == "hello\nworld");

    std::cout << "testStringLiteral: PASS\n";
}

static void testOperators() {
    Lexer lex("-> => .. ..= ... :: *= += -= /= <<= >>= **", "<test>");
    auto toks = lex.tokenizeAll();

    assert(toks[0].is(TokenKind::Arrow));
    assert(toks[1].is(TokenKind::FatArrow));
    assert(toks[2].is(TokenKind::DotDot));
    assert(toks[3].is(TokenKind::DotDotEq));
    assert(toks[4].is(TokenKind::Ellipsis));
    assert(toks[5].is(TokenKind::DoubleColon));
    assert(toks[6].is(TokenKind::StarEq));
    assert(toks[7].is(TokenKind::PlusEq));
    assert(toks[8].is(TokenKind::MinusEq));
    assert(toks[9].is(TokenKind::SlashEq));
    assert(toks[10].is(TokenKind::LtLtEq));
    assert(toks[11].is(TokenKind::GtGtEq));
    assert(toks[12].is(TokenKind::StarStar));

    std::cout << "testOperators: PASS\n";
}

static void testCommentSkipping() {
    Lexer lex("42 // line comment\n + /* block\ncomment */ 10", "<test>");
    auto toks = lex.tokenizeAll();

    assert(toks[0].is(TokenKind::LitInt));
    assert(toks[1].is(TokenKind::Plus));
    assert(toks[2].is(TokenKind::LitInt));

    std::cout << "testCommentSkipping: PASS\n";
}

int main() {
    testBasicTokens();
    testNumberLiterals();
    testStringLiteral();
    testOperators();
    testCommentSkipping();
    std::cout << "All lexer tests passed!\n";
    return 0;
}
