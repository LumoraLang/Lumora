#include "lumora/IREmitter.h"
#include "lumora/Lexer.h"
#include "lumora/Parser.h"
#include "lumora/Sema.h"
#include <cassert>
#include <iostream>
#include <string>

using namespace lumora;

static std::string compile(std::string_view src) {
  Lexer lex(src, "<test>");
  Parser parser(lex);
  auto mod = parser.parseModule("<test>");

  if (parser.hasErrors()) {
    for (auto &e : parser.errors())
      std::cerr << "parse error: " << e.message << "\n";
    return "";
  }

  Sema sema;
  sema.analyze(*mod);

  IREmitter emitter(sema);
  return emitter.emit(*mod);
}

static void testSimpleFn() {
  auto ir = compile("fn add(a: i64, b: i64) -> i64 { return a + b; }");
  assert(!ir.empty());
  assert(ir.find("define i64 @add(") != std::string::npos);
  assert(ir.find("add i64") != std::string::npos);
  assert(ir.find("ret i64") != std::string::npos);
  std::cout << "testSimpleFn: PASS\n";
}

static void testIfBranch() {
  auto ir = compile(R"(
        fn abs(x: i64) -> i64 {
            if x < 0 { return -x; }
            return x;
        }
    )");
  assert(!ir.empty());
  assert(ir.find("icmp slt") != std::string::npos);
  assert(ir.find("br i1") != std::string::npos);
  std::cout << "testIfBranch: PASS\n";
}

static void testWhileLoop() {
  auto ir = compile(R"(
        fn countdown(n: i64) -> i64 {
            let mut i = n;
            while i > 0 {
                i = i - 1;
            }
            return i;
        }
    )");
  assert(!ir.empty());
  assert(ir.find("while.hdr") != std::string::npos);
  assert(ir.find("while.body") != std::string::npos);
  assert(ir.find("while.exit") != std::string::npos);
  std::cout << "testWhileLoop: PASS\n";
}

static void testStringLit() {
  auto ir = compile(R"(
        fn get_msg() -> *u8 {
            return "hello";
        }
    )");
  assert(!ir.empty());
  assert(ir.find("@.str.") != std::string::npos);
  assert(ir.find("getelementptr") != std::string::npos);
  std::cout << "testStringLit: PASS\n";
}

static void testExternDecl() {
  auto ir = compile(R"(
        extern "C" {
            fn printf(fmt: *u8, ...) -> i32;
        }
        fn main() -> i32 {
            return 0;
        }
    )");
  assert(!ir.empty());
  assert(ir.find("declare i32 @printf(i8*, ...)") != std::string::npos);
  std::cout << "testExternDecl: PASS\n";
}

static void testStructDecl() {
  auto ir = compile(R"(
        struct Point {
            x: f64,
            y: f64,
        }
        fn origin() -> i64 {
            return 0;
        }
    )");
  assert(!ir.empty());
  assert(ir.find("%struct.Point = type { double, double }") !=
         std::string::npos);
  std::cout << "testStructDecl: PASS\n";
}

static void testCastExpr() {
  auto ir = compile(R"(
        fn to_f64(x: i64) -> f64 {
            return x as f64;
        }
    )");
  assert(!ir.empty());
  assert(ir.find("sitofp") != std::string::npos);
  std::cout << "testCastExpr: PASS\n";
}

int main() {
  testSimpleFn();
  testIfBranch();
  testWhileLoop();
  testStringLit();
  testExternDecl();
  testStructDecl();
  testCastExpr();
  std::cout << "All codegen tests passed!\n";
  return 0;
}
