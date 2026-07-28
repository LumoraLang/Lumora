#pragma once
#include "lumora/Config.h"
#include "lumora/Lexer.h"
#include "lumora/Parser.h"
#include "lumora/Sema.h"
#include "lumora/IREmitter.h"
#include "lumora/ExtensionHost.h"
#include <filesystem>
#include <string>
#include <vector>
#include <memory>
#include <functional>

namespace lumora {

struct CompileResult {
    bool        success  = false;
    std::string irOutput;
    std::string irPath;
    std::string sourceFile;
};

struct PipelineOptions {
    bool dumpTokens  = false;
    bool dumpAST     = false;
    bool dumpIR      = false;
    bool stopAfterIR = false;
    bool noOpt       = false;
    bool verbose     = false;
    bool multiboot   = false;
    uint32_t bootBase = 0x100000;
    std::string targetTriple;
    std::string dataLayout;
};

class Pipeline {
public:
    explicit Pipeline(LumoraConfig cfg, PipelineOptions opts = {});

    bool run();

    CompileResult compileFile(const std::filesystem::path& srcPath);

    bool runOpt(const OptStep& step);
    bool runLink(const LinkStep& step);
    bool runCommand(const CommandStep& step);

    void loadExtensions();

    [[nodiscard]] const LumoraConfig&    config()     const noexcept;
    [[nodiscard]] const PipelineOptions& options()    const noexcept;
    [[nodiscard]] ExtensionHost&         extHost()    noexcept;

private:
    LumoraConfig    m_cfg;
    PipelineOptions m_opts;
    ExtensionHost   m_extHost;

    std::filesystem::path outputPath(const std::filesystem::path& src) const;
    void ensureOutputDir() const;
    void log(std::string_view msg) const;
    bool execCmd(const std::string& cmd) const;
};

} 
