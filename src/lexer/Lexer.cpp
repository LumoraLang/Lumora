#include "lumora/Lexer.h"
#include <algorithm>
#include <cctype>
#include <filesystem>
#include <format>
#include <fstream>
#include <regex>
#include <sstream>
#include <stdexcept>

namespace lumora {

namespace {
static const std::unordered_map<std::string_view, TokenKind> kKeywords = {
    {"fn", TokenKind::KwFn},
    {"let", TokenKind::KwLet},
    {"mut", TokenKind::KwMut},
    {"const", TokenKind::KwConst},
    {"return", TokenKind::KwReturn},
    {"if", TokenKind::KwIf},
    {"else", TokenKind::KwElse},
    {"while", TokenKind::KwWhile},
    {"for", TokenKind::KwFor},
    {"in", TokenKind::KwIn},
    {"break", TokenKind::KwBreak},
    {"continue", TokenKind::KwContinue},
    {"struct", TokenKind::KwStruct},
    {"enum", TokenKind::KwEnum},
    {"union", TokenKind::KwUnion},
    {"impl", TokenKind::KwImpl},
    {"trait", TokenKind::KwTrait},
    {"type", TokenKind::KwType},
    {"use", TokenKind::KwUse},
    {"mod", TokenKind::KwMod},
    {"pub", TokenKind::KwPub},
    {"extern", TokenKind::KwExtern},
    {"true", TokenKind::KwTrue},
    {"false", TokenKind::KwFalse},
    {"null", TokenKind::KwNull},
    {"as", TokenKind::KwAs},
    {"is", TokenKind::KwIs},
    {"match", TokenKind::KwMatch},
    {"defer", TokenKind::KwDefer},
    {"unsafe", TokenKind::KwUnsafe},
    {"inline", TokenKind::KwInline},
    {"volatile", TokenKind::KwVolatile},
    {"async", TokenKind::KwAsync},
    {"await", TokenKind::KwAwait},
    {"yield", TokenKind::KwYield},
    {"sizeof", TokenKind::KwSizeof},
    {"alignof", TokenKind::KwAlignof},
    {"typeof", TokenKind::KwTypeof},
    {"offsetof", TokenKind::KwOffsetof},
};
}

Lexer::Lexer(std::string_view source, std::string_view filename)
    : m_src(source), m_file(filename) {}

void Lexer::registerExtension(LexerExtensionPoint ext) {
  m_extensions.push_back(std::move(ext));
}

SourceLoc Lexer::makeLoc() const noexcept {
  return {m_line, m_col, static_cast<uint32_t>(m_pos), m_file};
}

Token Lexer::makeToken(TokenKind k, std::string raw, SourceLoc loc) const {
  return Token{k, std::move(raw), loc, {}};
}

char Lexer::cur() const noexcept {
  return m_pos < m_src.size() ? m_src[m_pos] : '\0';
}

char Lexer::lookahead(size_t n) const noexcept {
  return (m_pos + n) < m_src.size() ? m_src[m_pos + n] : '\0';
}

char Lexer::eat() noexcept {
  char c = cur();
  m_pos++;
  if (c == '\n') {
    m_line++;
    m_col = 1;
  } else {
    m_col++;
  }
  return c;
}

bool Lexer::match(char c) noexcept {
  if (cur() == c) {
    eat();
    return true;
  }
  return false;
}

bool Lexer::match(std::string_view sv) noexcept {
  if (m_src.substr(m_pos, sv.size()) == sv) {
    for (size_t i = 0; i < sv.size(); ++i)
      eat();
    return true;
  }
  return false;
}

void Lexer::skipWhitespaceAndComments() {
  while (true) {
    while (std::isspace(static_cast<unsigned char>(cur())))
      eat();

    if (cur() == '/' && lookahead() == '/') {
      while (cur() != '\n' && cur() != '\0')
        eat();
      continue;
    }

    if (cur() == '/' && lookahead() == '*') {
      eat();
      eat();
      while (!(cur() == '*' && lookahead() == '/') && cur() != '\0')
        eat();
      if (cur() != '\0') {
        eat();
        eat();
      }
      continue;
    }

    break;
  }
}

std::vector<Token> Lexer::tryExtensions(size_t startPos) {
  std::string_view remaining = std::string_view(m_src).substr(startPos);
  for (auto &ext : m_extensions) {
    std::regex re("^(?:" + ext.pattern + ")");
    std::cmatch m;
    if (std::regex_search(remaining.data(), remaining.data() + remaining.size(),
                          m, re)) {
      std::string_view matched(remaining.data(), m[0].length());
      auto loc = makeLoc();
      for (size_t i = 0; i < matched.size(); ++i)
        eat();
      return ext.handler(matched, loc);
    }
  }
  return {};
}

Token Lexer::lexIdOrKeyword() {
  auto loc = makeLoc();
  std::string buf;
  while (std::isalnum(static_cast<unsigned char>(cur())) || cur() == '_') {
    buf += eat();
  }
  auto it = kKeywords.find(buf);
  if (it != kKeywords.end())
    return makeToken(it->second, buf, loc);
  return makeToken(TokenKind::Ident, buf, loc);
}

Token Lexer::lexNumber() {
  auto loc = makeLoc();
  std::string buf;
  bool isFloat = false;

  if (cur() == '0' && (lookahead() == 'x' || lookahead() == 'X')) {
    buf += eat();
    buf += eat();
    while (std::isxdigit(static_cast<unsigned char>(cur())) || cur() == '_') {
      if (cur() != '_')
        buf += cur();
      eat();
    }
    Token t = makeToken(TokenKind::LitInt, buf, loc);
    t.extra = static_cast<int64_t>(std::stoll(buf.substr(2), nullptr, 16));
    return t;
  }

  if (cur() == '0' && (lookahead() == 'b' || lookahead() == 'B')) {
    buf += eat();
    buf += eat();
    while (cur() == '0' || cur() == '1' || cur() == '_') {
      if (cur() != '_')
        buf += cur();
      eat();
    }
    Token t = makeToken(TokenKind::LitInt, buf, loc);
    t.extra = static_cast<int64_t>(std::stoll(buf.substr(2), nullptr, 2));
    return t;
  }

  while (std::isdigit(static_cast<unsigned char>(cur())) || cur() == '_') {
    if (cur() != '_')
      buf += cur();
    eat();
  }

  if (cur() == '.' && std::isdigit(static_cast<unsigned char>(lookahead()))) {
    isFloat = true;
    buf += eat();
    while (std::isdigit(static_cast<unsigned char>(cur())) || cur() == '_') {
      if (cur() != '_')
        buf += cur();
      eat();
    }
  }

  if (cur() == 'e' || cur() == 'E') {
    isFloat = true;
    buf += eat();
    if (cur() == '+' || cur() == '-')
      buf += eat();
    while (std::isdigit(static_cast<unsigned char>(cur())))
      buf += eat();
  }

  if (isFloat) {
    Token t = makeToken(TokenKind::LitFloat, buf, loc);
    t.extra = std::stod(buf);
    return t;
  }
  Token t = makeToken(TokenKind::LitInt, buf, loc);
  t.extra = static_cast<int64_t>(std::stoll(buf));
  return t;
}

Token Lexer::lexString() {
  auto loc = makeLoc();
  eat();
  std::string val;
  while (cur() != '"' && cur() != '\0') {
    if (cur() == '\\') {
      eat();
      switch (eat()) {
      case 'n':
        val += '\n';
        break;
      case 't':
        val += '\t';
        break;
      case 'r':
        val += '\r';
        break;
      case 'e':
        val += '\033';
        break;
      case '\\':
        val += '\\';
        break;
      case '"':
        val += '"';
        break;
      case '0':
        val += '\0';
        break;
      default:
        val += '?';
        break;
      }
    } else {
      val += eat();
    }
  }
  if (cur() == '"')
    eat();
  Token t = makeToken(TokenKind::LitString, "\"" + val + "\"", loc);
  t.extra = val;
  return t;
}

Token Lexer::lexChar() {
  auto loc = makeLoc();
  eat();
  char val = 0;
  if (cur() == '\\') {
    eat();
    switch (eat()) {
    case 'n':
      val = '\n';
      break;
    case 't':
      val = '\t';
      break;
    case 'r':
      val = '\r';
      break;
    case 'e':
      val = '\033';
      break;
    case '0':
      val = '\0';
      break;
    default:
      val = '?';
      break;
    }
  } else {
    val = eat();
  }
  if (cur() == '\'')
    eat();
  Token t = makeToken(TokenKind::LitChar, std::string(1, val), loc);
  t.extra = static_cast<int64_t>(val);
  return t;
}

Token Lexer::lexSymbol() {
  auto loc = makeLoc();

  auto eat2 = [&](TokenKind k, const char *raw) -> Token {
    eat();
    eat();
    return makeToken(k, raw, loc);
  };
  auto eat1 = [&](TokenKind k, const char *raw) -> Token {
    eat();
    return makeToken(k, raw, loc);
  };

  switch (cur()) {
  case '(':
    return eat1(TokenKind::LParen, "(");
  case ')':
    return eat1(TokenKind::RParen, ")");
  case '{':
    return eat1(TokenKind::LBrace, "{");
  case '}':
    return eat1(TokenKind::RBrace, "}");
  case '[':
    return eat1(TokenKind::LBracket, "[");
  case ']':
    return eat1(TokenKind::RBracket, "]");
  case ',':
    return eat1(TokenKind::Comma, ",");
  case ';':
    return eat1(TokenKind::Semicolon, ";");
  case '~':
    return eat1(TokenKind::Tilde, "~");
  case '@':
    return eat1(TokenKind::At, "@");
  case '#':
    return eat1(TokenKind::Hash, "#");
  case '$':
    return eat1(TokenKind::Dollar, "$");
  case '?':
    return eat1(TokenKind::Question, "?");

  case ':':
    if (lookahead() == ':')
      return eat2(TokenKind::DoubleColon, "::");
    return eat1(TokenKind::Colon, ":");
  case '.':
    if (lookahead() == '.' && lookahead(2) == '.') {
      eat();
      eat();
      eat();
      return makeToken(TokenKind::Ellipsis, "...", loc);
    }
    if (lookahead() == '.' && lookahead(2) == '=') {
      eat();
      eat();
      eat();
      return makeToken(TokenKind::DotDotEq, "..=", loc);
    }
    if (lookahead() == '.')
      return eat2(TokenKind::DotDot, "..");
    return eat1(TokenKind::Dot, ".");
  case '=':
    if (lookahead() == '=')
      return eat2(TokenKind::EqEq, "==");
    if (lookahead() == '>')
      return eat2(TokenKind::FatArrow, "=>");
    return eat1(TokenKind::Eq, "=");
  case '!':
    if (lookahead() == '=')
      return eat2(TokenKind::BangEq, "!=");
    return eat1(TokenKind::Bang, "!");
  case '<':
    if (lookahead() == '<') {
      if (lookahead(2) == '=') {
        eat();
        eat();
        eat();
        return makeToken(TokenKind::LtLtEq, "<<=", loc);
      }
      return eat2(TokenKind::LtLt, "<<");
    }
    if (lookahead() == '=')
      return eat2(TokenKind::LtEq, "<=");
    return eat1(TokenKind::Lt, "<");
  case '>':
    if (lookahead() == '>') {
      if (lookahead(2) == '=') {
        eat();
        eat();
        eat();
        return makeToken(TokenKind::GtGtEq, ">>=", loc);
      }
      return eat2(TokenKind::GtGt, ">>");
    }
    if (lookahead() == '=')
      return eat2(TokenKind::GtEq, ">=");
    return eat1(TokenKind::Gt, ">");
  case '+':
    if (lookahead() == '+')
      return eat2(TokenKind::PlusPlus, "++");
    if (lookahead() == '=')
      return eat2(TokenKind::PlusEq, "+=");
    return eat1(TokenKind::Plus, "+");
  case '-':
    if (lookahead() == '-')
      return eat2(TokenKind::MinusMinus, "--");
    if (lookahead() == '=')
      return eat2(TokenKind::MinusEq, "-=");
    if (lookahead() == '>')
      return eat2(TokenKind::Arrow, "->");
    return eat1(TokenKind::Minus, "-");
  case '*':
    if (lookahead() == '*')
      return eat2(TokenKind::StarStar, "**");
    if (lookahead() == '=')
      return eat2(TokenKind::StarEq, "*=");
    return eat1(TokenKind::Star, "*");
  case '/':
    if (lookahead() == '=')
      return eat2(TokenKind::SlashEq, "/=");
    return eat1(TokenKind::Slash, "/");
  case '%':
    if (lookahead() == '=')
      return eat2(TokenKind::PercentEq, "%=");
    return eat1(TokenKind::Percent, "%");
  case '&':
    if (lookahead() == '&')
      return eat2(TokenKind::AmpAmp, "&&");
    if (lookahead() == '=')
      return eat2(TokenKind::AmpEq, "&=");
    return eat1(TokenKind::Amp, "&");
  case '|':
    if (lookahead() == '|')
      return eat2(TokenKind::PipePipe, "||");
    if (lookahead() == '=')
      return eat2(TokenKind::PipeEq, "|=");
    return eat1(TokenKind::Pipe, "|");
  case '^':
    if (lookahead() == '=')
      return eat2(TokenKind::CaretEq, "^=");
    return eat1(TokenKind::Caret, "^");
  default:
    break;
  }

  auto raw = std::string(1, eat());
  throw std::runtime_error(std::format("{}:{}:{}: unexpected character '{}'",
                                       m_file, loc.line, loc.col, raw));
}

Token Lexer::advance() {
  for (;;) {
    if (!m_injected.empty()) {
      auto t = m_injected.front();
      m_injected.erase(m_injected.begin());
      return t;
    }

    skipWhitespaceAndComments();
    if (cur() == '\0')
      return makeToken(TokenKind::Eof, "", makeLoc());
    if (m_pos + 8 <= m_src.size() && m_src.substr(m_pos, 8) == "@include") {
      auto loc = makeLoc();
      for (int i = 0; i < 8; ++i)
        eat();
      skipWhitespaceAndComments();
      if (cur() == '"') {
        auto pathTok = lexString();
        std::string incPath = std::get<std::string>(pathTok.extra);
        std::filesystem::path currentPath(m_file);
        auto fullPath =
            (currentPath.parent_path() / incPath).lexically_normal();
        std::string canonical = fullPath.string();
        if (m_includedSet.count(canonical)) {
          throw std::runtime_error(
              std::format("{}:{}:{}: circular include detected for '{}'",
                          m_file, loc.line, loc.col, canonical));
        }

        std::ifstream f(fullPath);
        if (f) {
          std::stringstream ss;
          ss << f.rdbuf();
          std::string content = ss.str();
          m_includedFiles.push_back(canonical);
          m_includedSet.insert(canonical);
          Lexer subLexer(content, m_includedFiles.back());
          subLexer.m_includedSet = m_includedSet;
          for (const auto &ext : m_extensions) {
            subLexer.registerExtension(ext);
          }

          auto subToks = subLexer.tokenizeAll();
          for (auto &t : subToks) {
            if (!t.isEof()) {
              m_injected.push_back(t);
            }
          }
          continue;
        } else {
          throw std::runtime_error(
              std::format("{}:{}:{}: cannot open included file '{}'", m_file,
                          loc.line, loc.col, fullPath.string()));
        }
      } else {
        throw std::runtime_error(
            std::format("{}:{}:{}: expected string literal after @include",
                        m_file, loc.line, loc.col));
      }
    }

    auto extToks = tryExtensions(m_pos);
    if (!extToks.empty()) {
      for (auto &t : extToks)
        m_injected.push_back(t);
      continue;
    }

    if (std::isalpha(static_cast<unsigned char>(cur())) || cur() == '_')
      return lexIdOrKeyword();
    if (std::isdigit(static_cast<unsigned char>(cur())))
      return lexNumber();
    if (cur() == '"')
      return lexString();
    if (cur() == '\'')
      return lexChar();

    return lexSymbol();
  }
}

Token Lexer::next() {
  if (!m_peekBuf.empty()) {
    auto t = m_peekBuf.front();
    m_peekBuf.erase(m_peekBuf.begin());
    return t;
  }
  return advance();
}

Token Lexer::peek(size_t offset) {
  while (m_peekBuf.size() <= offset)
    m_peekBuf.push_back(advance());
  return m_peekBuf[offset];
}

std::vector<Token> Lexer::tokenizeAll() {
  std::vector<Token> toks;
  while (true) {
    auto t = next();
    toks.push_back(t);
    if (t.isEof())
      break;
  }
  return toks;
}

SourceLoc Lexer::currentLoc() const noexcept { return makeLoc(); }

} // namespace lumora
