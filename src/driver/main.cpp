#include "lumora/Config.h"
#include "lumora/Pipeline.h"
#include <filesystem>
#include <iostream>
#include <span>
#include <string>
#include <vector>
static void printUsage(std::string_view argv0) {
  std::cerr << "Usage: " << argv0
            << " [options] [files...]\n"
               "Options:\n"
               "  --conf <file>     Use specified lumora.conf (default: "
               "lumora.conf)\n"
               "  --dump-tokens     Print tokens and exit\n"
               "  --dump-ast        Print AST and exit\n"
               "  --dump-ir         Print generated LLVM IR\n"
               "  --stop-ir         Stop after IR emission (no opt/link)\n"
               "  --multiboot       Generate multiboot boot stub (.boot.S)\n"
               "  --no-opt          Skip opt step\n"
               "  --verbose         Verbose build output\n"
               "  --ext-dir <dir>   Load extensions from directory\n"
               "  --output <dir>    Override output directory\n"
               "  -h, --help        Print this help\n";
}

int main(int argc, char **argv) {
  std::vector<std::string_view> args(argv + 1, argv + argc);
  lumora::PipelineOptions opts;
  std::string confPath = "lumora.conf";
  std::vector<std::string> files;
  std::vector<std::string> extDirs;
  std::string outputDir;
  for (size_t i = 0; i < args.size(); ++i) {
    auto a = args[i];
    if (a == "--dump-tokens") {
      opts.dumpTokens = true;
    } else if (a == "--dump-ast") {
      opts.dumpAST = true;
    } else if (a == "--dump-ir") {
      opts.dumpIR = true;
    } else if (a == "--stop-ir") {
      opts.stopAfterIR = true;
    } else if (a == "--multiboot") {
      opts.multiboot = true;
    } else if (a == "--no-opt") {
      opts.noOpt = true;
    } else if (a == "--verbose" || a == "-v") {
      opts.verbose = true;
    } else if ((a == "--conf" || a == "-c") && i + 1 < args.size()) {
      confPath = args[++i];
    } else if (a == "--ext-dir" && i + 1 < args.size()) {
      extDirs.push_back(std::string(args[++i]));
    } else if (a == "--output" && i + 1 < args.size()) {
      outputDir = args[++i];
    } else if (a == "-h" || a == "--help") {
      printUsage(argv[0]);
      return 0;
    } else if (!a.starts_with('-')) {
      files.push_back(std::string(a));
    } else {
      std::cerr << "unknown option: " << a << "\n";
      printUsage(argv[0]);
      return 1;
    }
  }

  lumora::LumoraConfig cfg;
  std::filesystem::path confDir;
  if (std::filesystem::exists(confPath)) {
    try {
      cfg = lumora::LumoraConfig::loadFromFile(confPath);
    } catch (const std::exception &e) {
      std::cerr << "error loading config: " << e.what() << "\n";
      return 1;
    }
    confDir = std::filesystem::absolute(confPath).parent_path();
  }

  for (auto &d : extDirs) cfg.extensionDirs.push_back(d);
  if (!outputDir.empty()) cfg.outputDir = outputDir;
  if (cfg.multiboot) opts.multiboot = true;
  if (!confDir.empty()) {
    cfg.outputDir = (confDir / cfg.outputDir).string();
    for (auto &grp : cfg.groups)
      for (auto &f : grp.files)
        f = (confDir / f).string();
    for (auto &opt : cfg.optSteps) {
      if (!opt.input.empty() && !opt.input.starts_with('-'))
        opt.input = (confDir / opt.input).string();
      if (!opt.output.empty() && !opt.output.starts_with('-'))
        opt.output = (confDir / opt.output).string();
    }
    for (auto &lnk : cfg.linkSteps) {
      if (!lnk.script.empty() && !lnk.script.starts_with('-'))
        lnk.script = (confDir / lnk.script).string();
      for (auto &i : lnk.inputs)
        if (!i.starts_with('-'))
          i = (confDir / i).string();
      if (!lnk.output.empty() && !lnk.output.starts_with('-'))
        lnk.output = (confDir / lnk.output).string();
    }
    for (auto &cmd : cfg.commandSteps)
      cmd.cmd = "cd " + confDir.string() + " && " + cmd.cmd;
  }

  if (!files.empty()) {
    if (cfg.groups.empty())
      cfg.groups.push_back({"cli", {}, {}});
    for (auto &f : files)
      cfg.groups.front().files.push_back(f);
  }

  if (cfg.groups.empty() || cfg.groups.front().files.empty()) {
    std::cerr << "lumorac: \x1b[31merror\x1b[0m: no input files\n";
    printUsage(argv[0]);
    return 1;
  }

  lumora::Pipeline pipeline(std::move(cfg), opts);
  if (!pipeline.run()) {
    std::cerr << "lumorac: \x1b[31merror\x1b[0m: build failed\n";
    return 1;
  }

  return 0;
}
