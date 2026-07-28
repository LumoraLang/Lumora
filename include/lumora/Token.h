#pragma once
#include <string>
#include <string_view>
#include <cstdint>
#include <variant>
namespace lumora {
enum class TokenKind : uint32_t {
    Eof = 0,
    LParen, RParen, LBrace, RBrace, LBracket, RBracket,
    Comma, Semicolon, Colon, DoubleColon, Dot, Arrow,
    FatArrow, DotDot, DotDotEq, Ellipsis,
    Amp, AmpAmp, Pipe, PipePipe, Caret, Tilde,
    Bang, BangEq,
    Lt, LtLt, LtEq,
    Gt, GtGt, GtEq,
    Eq, EqEq,
    Plus, PlusEq, PlusPlus,
    Minus, MinusEq, MinusMinus,
    Star, StarEq, StarStar,
    Slash, SlashEq,
    Percent, PercentEq,
    AmpEq, PipeEq, CaretEq, LtLtEq, GtGtEq,
    At, Hash, Dollar, Question,
    KwFn, KwLet, KwMut, KwConst, KwReturn, KwIf, KwElse,
    KwWhile, KwFor, KwIn, KwBreak, KwContinue,
    KwStruct, KwEnum, KwUnion, KwImpl, KwTrait,
    KwType, KwUse, KwMod, KwPub, KwExtern,
    KwTrue, KwFalse, KwNull, KwAs, KwIs,
    KwMatch, KwDefer, KwUnsafe, KwInline,
    KwVolatile, KwAsync, KwAwait, KwYield,
    KwSizeof, KwAlignof, KwTypeof, KwOffsetof,
    KwAsm,
    Ident,
    LitInt, LitFloat, LitString, LitChar, LitBool,
    ExtensionToken,
    _Count
};

struct SourceLoc {
    uint32_t line   = 1;
    uint32_t col    = 1;
    uint32_t offset = 0;
    std::string_view file;
};

struct Token {
    TokenKind    kind;
    std::string  raw;
    SourceLoc    loc;
    using ExtraData = std::variant<std::monostate, int64_t, double, std::string, uint32_t>;
    ExtraData extra{};
    [[nodiscard]] bool is(TokenKind k)  const noexcept { return kind == k; }
    [[nodiscard]] bool isKeyword()      const noexcept { return kind >= TokenKind::KwFn && kind <= TokenKind::KwAsm; }
    [[nodiscard]] bool isLiteral()      const noexcept { return kind >= TokenKind::LitInt && kind <= TokenKind::LitBool; }
    [[nodiscard]] bool isIdent()        const noexcept { return kind == TokenKind::Ident; }
    [[nodiscard]] bool isEof()          const noexcept { return kind == TokenKind::Eof; }
    [[nodiscard]] std::string_view kindName() const noexcept;
};

std::string_view tokenKindName(TokenKind k) noexcept;

}
