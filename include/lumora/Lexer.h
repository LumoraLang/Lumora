#pragma once
#include "lumora/Token.h"
#include <string>
#include <string_view>
#include <vector>
#include <functional>
#include <span>
#include <list>
#include <unordered_set>
namespace lumora {
struct LexerExtensionPoint {
    std::string pattern;
    std::function<std::vector<Token>(std::string_view matched, SourceLoc loc)> handler;
};

class Lexer {
public:
    explicit Lexer(std::string_view source, std::string_view filename = "<input>");
    void registerExtension(LexerExtensionPoint ext);
    Token next();
    Token peek(size_t offset = 0);
    std::vector<Token> tokenizeAll();
    [[nodiscard]] size_t position() const noexcept { return m_pos; }
    [[nodiscard]] SourceLoc currentLoc() const noexcept;

private:
    std::string        m_src;
    std::string_view   m_file;
    size_t             m_pos  = 0;
    uint32_t           m_line = 1;
    uint32_t           m_col  = 1;
    std::vector<LexerExtensionPoint> m_extensions;
    std::vector<Token>               m_peekBuf;
    std::list<std::string>           m_includedFiles;
    std::unordered_set<std::string>  m_includedSet;
    std::vector<Token>               m_injected;
    Token        advance();
    Token        lexIdOrKeyword();
    Token        lexNumber();
    Token        lexString();
    Token        lexChar();
    Token        lexSymbol();
    void         skipWhitespaceAndComments();
    char         cur()                      const noexcept;
    char         lookahead(size_t n = 1)    const noexcept;
    char         eat()                      noexcept;
    bool         match(char c)              noexcept;
    bool         match(std::string_view sv) noexcept;
    SourceLoc    makeLoc()                  const noexcept;
    Token        makeToken(TokenKind k, std::string raw, SourceLoc loc) const;
    std::vector<Token> tryExtensions(size_t startPos);
};
}
