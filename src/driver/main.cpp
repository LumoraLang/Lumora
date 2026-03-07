#include "lumora/Pipeline.h"
#include "lumora/Config.h"
#include <iostream>
#include <string>
#include <vector>
#include <filesystem>
#include <span>

static void printUsage(std::string_view argv0) {
    std::cerr <<
        "Usage: " << argv0 << " [options] [files...]\n"
        "Options:\n"
        "  --conf <file>     Use specified lumora.conf (default: lumora.conf)\n"
        "  --dump-tokens     Print tokens and exit\n"
        "  --dump-ast        Print AST and exit\n"
        "  --dump-ir         Print generated LLVM IR\n"
        "  --stop-ir         Stop after IR emission (no opt/link)\n"
        "  --no-opt          Skip opt step\n"
        "  --verbose         Verbose build output\n"
        "  --ext-dir <dir>   Load extensions from directory\n"
        "  --output <dir>    Override output directory\n"
        "  -h, --help        Print this help\n";
}

int main(int argc, char** argv) {
    std::vector<std::string_view> args(argv + 1, argv + argc);

    lumora::PipelineOptions opts;
    std::string             confPath = "lumora.conf";
    std::vector<std::string> files;
    std::vector<std::string> extDirs;
    std::string              outputDir;

    for (size_t i = 0; i < args.size(); ++i) {
        auto a = args[i];
        if (a == "--dump-tokens")   { opts.dumpTokens  = true; }
        else if (a == "--dump-ast") { opts.dumpAST     = true; }
        else if (a == "--dump-ir")  { opts.dumpIR      = true; }
        else if (a == "--stop-ir")  { opts.stopAfterIR = true; }
        else if (a == "--no-opt")   { opts.noOpt       = true; }
        else if (a == "--verbose" || a == "-v") { opts.verbose = true; }
        else if ((a == "--conf" || a == "-c") && i + 1 < args.size()) {
            confPath = args[++i];
        }
        else if (a == "--ext-dir" && i + 1 < args.size()) {
            extDirs.push_back(std::string(args[++i]));
        }
        else if (a == "--output" && i + 1 < args.size()) {
            outputDir = args[++i];
        }
        else if (a == "-h" || a == "--help") {
            printUsage(argv[0]);
            return 0;
        }
        else if (!a.starts_with('-')) {
            files.push_back(std::string(a));
        }
        else {
            std::cerr << "unknown option: " << a << "\n";
            printUsage(argv[0]);
            return 1;
        }
    }

    lumora::LumoraConfig cfg;

    if (std::filesystem::exists(confPath)) {
        try {
            cfg = lumora::LumoraConfig::loadFromFile(confPath);
        } catch (const std::exception& e) {
            std::cerr << "error loading config: " << e.what() << "\n";
            return 1;
        }
    }

    for (auto& d : extDirs) cfg.extensionDirs.push_back(d);
    if (!outputDir.empty())  cfg.outputDir = outputDir;

    if (!files.empty()) {
        if (cfg.groups.empty()) cfg.groups.push_back({"cli", {}, {}});
        for (auto& f : files) cfg.groups.front().files.push_back(f);
    }

    if (cfg.groups.empty() || cfg.groups.front().files.empty()) {
        std::cerr << "lumora: no input files\n";
        printUsage(argv[0]);
        return 1;
    }

    lumora::Pipeline pipeline(std::move(cfg), opts);

    if (!pipeline.run()) {
        std::cerr << "lumora: build failed\n";
        return 1;
    }

    return 0;
}
