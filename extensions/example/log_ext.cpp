#include "lumora/ExtensionHost.h"
#include "lumora/Parser.h"
#include "lumora/IREmitter.h"
#include "lumora/AST.h"
#include <format>
#include <string>
#include <iostream>
#include <vector>
using namespace lumora;
using namespace lumora::ast;
static ExtensionAPI* gAPI = nullptr;
static NodePtr parseLogCall(Parser& parser, Token trigger) {
    auto node = std::make_unique<ExtensionNode>();
    node->extensionId = "log";
    node->loc         = trigger.loc;
    node->tokens.push_back(trigger);
    parser.expect(TokenKind::LParen);
    while (!parser.check(TokenKind::RParen) && !parser.check(TokenKind::Eof)) {
        node->children.push_back(parser.parseExpr());
        if (!parser.match(TokenKind::Comma)) break;
    }
    parser.expect(TokenKind::RParen);
    parser.match(TokenKind::Semicolon);
    return node;
}

static NodePtr parseAssertCall(Parser& parser, Token trigger) {
    auto node = std::make_unique<ExtensionNode>();
    node->extensionId = "assert";
    node->loc         = trigger.loc;
    node->tokens.push_back(trigger);
    parser.expect(TokenKind::LParen);
    node->children.push_back(parser.parseExpr());
    if (parser.match(TokenKind::Comma)) {
        node->children.push_back(parser.parseExpr());
    }
    parser.expect(TokenKind::RParen);
    parser.match(TokenKind::Semicolon);
    return node;
}

static std::string emitLogNode(ExtensionNode& node, IREmitter& emitter) {
    if (node.children.empty()) return "";
    emitter.emitInstr("; @log expansion");
    static uint32_t strIdx = 10000;
    std::string fmtStr;
    std::string argStr;
    for (size_t i = 0; i < node.children.size(); ++i) {
        if (i) fmtStr += " ";
        fmtStr += "%s";
    }
    fmtStr += "\\0A";
    auto fmtLen  = fmtStr.size() - 1 + 1;
    auto fmtName = std::format("@.log.fmt.{}", strIdx++);
    emitter.emitRaw(std::format("{} = private unnamed_addr constant [{} x i8] c\"{}\"\n", fmtName, fmtLen, fmtStr));
    std::string fmtReg = std::format("%logfmt{}", strIdx);
    emitter.emitInstr(std::format("{} = getelementptr [{} x i8], [{} x i8]* {}, i64 0, i64 0", fmtReg, fmtLen, fmtLen, fmtName));
    argStr = "i8* " + fmtReg;
    (void)emitter;
    return "";
}

static std::string emitAssertNode(ExtensionNode& node, IREmitter& emitter) {
    if (node.children.empty()) return "";
    auto condVal = emitter.emitExpr(*node.children[0]);
    std::string condReg = condVal.reg;
    if (condVal.type->kind != TypeKind::Bool) {
        auto cmpReg = emitter.newReg();
        std::string zero = "0";
        if (condVal.type->isFloat()) zero = "0.0";
        std::string op = "icmp ne";
        if (condVal.type->isFloat()) op = "fcmp one";
        emitter.emitInstr(std::format("{} = {} {} {}, {}", cmpReg, op, emitter.llvmType(condVal.type), condReg, zero));
        condReg = cmpReg;
    }

    auto failLabel    = emitter.newLabel("assert.fail");
    auto successLabel = emitter.newLabel("assert.success");
    emitter.emitInstr(std::format("br i1 {}, label %{}, label %{}", condReg, successLabel, failLabel));
    emitter.beginBlock(failLabel);
    std::string msg = "Assertion failed\n";
    if (node.children.size() > 1) {
        if (auto s = dynamic_cast<StringLit*>(node.children[1].get())) {
            msg = s->value + "\n";
        }
    }

    StringLit sLit;
    sLit.kind  = NodeKind::StringLit;
    sLit.value = msg;
    auto msgVal = emitter.emitExpr(sLit);
    emitter.emitInstr(std::format("call i32 (i8*, ...) @printf(i8* {})", msgVal.reg));
    emitter.emitInstr("call void @exit(i32 1)");
    emitter.emitInstr("unreachable");
    emitter.beginBlock(successLabel);
    return "";
}

extern "C" {
ExtensionManifest lumora_extension_init(ExtensionAPI& api) {
    gAPI = &api;
    if (api.parser) {
        api.parser->registerExtension(ParserExtensionPoint{
            "log",
            parseLogCall
        });

        api.parser->registerExtension(ParserExtensionPoint{
            "assert",
            parseAssertCall
        });
    }

    if (api.sema) {
        api.sema->registerExtension(SemaExtensionPoint{
            "log",
            [](ExtensionNode& node, Sema& sema) -> TypeRef {
                for (auto& c : node.children)
                    if (c) sema.inferExpr(*c);
                return SemaType::voidTy();
            }
        });

        api.sema->registerExtension(SemaExtensionPoint{
            "assert",
            [](ExtensionNode& node, Sema& sema) -> TypeRef {
                for (auto& c : node.children)
                    if (c) sema.inferExpr(*c);
                return SemaType::voidTy();
            }
        });
    }

    if (api.emitter) {
        api.emitter->registerExtension(CodegenExtensionPoint{
            "log",
            emitLogNode
        });

        api.emitter->registerExtension(CodegenExtensionPoint{
            "assert",
            emitAssertNode
        });
    }

    return ExtensionManifest{
        "lumora-log",
        "0.1.0",
        "Adds @log and @assert macros",
        {}
    };
}

void lumora_extension_destroy() {
    gAPI = nullptr;
}
}
