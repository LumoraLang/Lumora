#include "lumora/Lexer.h"
#include "lumora/Parser.h"
#include "lumora/Sema.h"
#include "lumora/IREmitter.h"
#include <iostream>
#include <string>
#include <sstream>
using namespace lumora;
static std::string compileToIR(const std::string& src) {
    Lexer lex(src, "<repl>");
    Parser parser(lex);
    auto mod = parser.parseModule("<repl>");
    if (parser.hasErrors()) {
        for (auto& e : parser.errors())
            std::cerr << "Parse error: " << e.message << "\n";
        return "";
    }

    Sema sema;
    sema.analyze(*mod);
    IREmitter emitter(sema);
    return emitter.emit(*mod);
}

int main() {
    std::cout << "=== Lumora REPL ===\n";
    std::cout << "Type 'exit' to quit.\n";
    std::string line;
    std::stringstream sourceBuffer;
    while (true) {
        std::cout << ">>> ";
        if (!std::getline(std::cin, line)) break;
        if (line == "exit") break;
        sourceBuffer << line << "\n";
        std::string ir = compileToIR(sourceBuffer.str());
        if (!ir.empty()) {
            std::cout << "--- LLVM IR ---\n";
            std::cout << ir << "\n";
        } else {
            std::cout << "(Waiting for more input...)\n";
        }
    }

    std::cout << "Goodbye!\n";
    return 0;
}
