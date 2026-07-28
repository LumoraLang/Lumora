#include "lumora/Token.h"
#include <array>

namespace lumora {

static constexpr std::array<std::string_view, static_cast<size_t>(TokenKind::_Count)> kNames = {
    "eof",
    "(", ")", "{", "}", "[", "]",
    ",", ";", ":", "::", ".", "->",
    "=>", "..", "..=", "...",
    "&", "&&", "|", "||", "^", "~",
    "!", "!=",
    "<", "<<", "<=",
    ">", ">>", ">=",
    "=", "==",
    "+", "+=", "++",
    "-", "-=", "--",
    "*", "*=", "**",
    "/", "/=",
    "%", "%=",
    "&=", "|=", "^=", "<<=", ">>=",
    "@", "#", "$", "?",
    "fn", "let", "mut", "const", "return", "if", "else",
    "while", "for", "in", "break", "continue",
    "struct", "enum", "union", "impl", "trait",
    "type", "use", "mod", "pub", "extern",
    "true", "false", "null", "as", "is",
    "match", "defer", "unsafe", "inline",
    "volatile", "async", "await", "yield",
    "sizeof", "alignof", "typeof", "offsetof", "asm",
    "<ident>",
    "<int>", "<float>", "<string>", "<char>", "<bool>",
    "<ext>",
};

std::string_view tokenKindName(TokenKind k) noexcept {
    auto i = static_cast<size_t>(k);
    if (i < kNames.size()) return kNames[i];
    return "<unknown>";
}

std::string_view Token::kindName() const noexcept {
    return tokenKindName(kind);
}

} 
