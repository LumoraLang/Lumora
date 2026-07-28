#pragma once
#include "lumora/Lexer.h"
#include "lumora/AST.h"
#include <functional>
#include <memory>
#include <vector>
#include <string>
#include <optional>

namespace lumora {

struct ParseError {
    std::string message;
    SourceLoc   loc;
};

struct ParserExtensionPoint {
    std::string              triggerPattern;
    std::function<ast::NodePtr(class Parser&, Token trigger)> handler;
};

class Parser {
public:
    explicit Parser(Lexer& lex);

    void registerExtension(ParserExtensionPoint ext);

    std::unique_ptr<ast::Module> parseModule(std::string_view filename);

    [[nodiscard]] const std::vector<ParseError>& errors() const noexcept;
    [[nodiscard]] bool hasErrors()                         const noexcept;

    Token             peek(size_t n = 0);
    Token             eat();
    bool              check(TokenKind k);
    bool              match(TokenKind k);
    Token             expect(TokenKind k, std::string_view msg = "");

    ast::NodePtr      parseTopLevelItem();
    ast::NodePtr      parseStmt();
    ast::NodePtr      parseExpr(int minPrec = 0);
    ast::TypePtr      parseType();

private:
    Lexer&                            m_lex;
    std::vector<ParserExtensionPoint> m_extensions;
    std::vector<ParseError>           m_errors;

    void error(std::string_view msg, SourceLoc loc = {});
    void synchronize();

    std::unique_ptr<ast::FnDecl>     parseFnDecl(bool isPub);
    std::unique_ptr<ast::StructDecl> parseStructDecl(bool isPub);
    std::unique_ptr<ast::EnumDecl>   parseEnumDecl(bool isPub);
    std::unique_ptr<ast::ImplDecl>   parseImplDecl();
    std::unique_ptr<ast::TraitDecl>  parseTraitDecl(bool isPub);
    std::unique_ptr<ast::TypeAlias>  parseTypeAlias(bool isPub);
    std::unique_ptr<ast::UseDecl>    parseUseDecl(bool isPub);
    std::unique_ptr<ast::ModDecl>    parseModDecl(bool isPub);
    std::unique_ptr<ast::ExternDecl> parseExternDecl();

    std::unique_ptr<ast::BlockStmt>  parseBlock();
    ast::NodePtr                     parseLetStmt(bool isPub = false);
    ast::NodePtr                     parseReturnStmt();
    ast::NodePtr                     parseIfStmt();
    ast::NodePtr                     parseWhileStmt();
    ast::NodePtr                     parseForStmt();
    ast::NodePtr                     parseMatchExpr();
    ast::NodePtr                     parseDeferStmt();
    ast::NodePtr                     parseAsmExpr();

    ast::NodePtr                     parsePrimaryExpr();
    ast::NodePtr                     parseUnaryExpr();
    ast::NodePtr                     parsePostfixExpr(ast::NodePtr base);
    int                              binOpPrec(TokenKind k) const noexcept;
    bool                             isAssignOp(TokenKind k) const noexcept;

    ast::TypePtr                     parsePrimType();
    ast::TypePtr                     parsePtrOrRefType();
    ast::TypePtr                     parseArrayOrSliceType();
    ast::TypePtr                     parseFnType();
    ast::TypePtr                     parseTupleType();
    std::vector<ast::Attribute>      parseAttributes();

    ast::NodePtr tryExtension();
};

} 
