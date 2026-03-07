#include "lumora/Pipeline.h"
#include "lumora/ASTDump.h"
#include <cstdlib>
#include <format>
#include <fstream>
#include <iostream>
#include <sstream>

namespace lumora {

Pipeline::Pipeline(LumoraConfig cfg, PipelineOptions opts)
    : m_cfg(std::move(cfg)), m_opts(std::move(opts)) {}

const LumoraConfig &Pipeline::config() const noexcept { return m_cfg; }
const PipelineOptions &Pipeline::options() const noexcept { return m_opts; }
ExtensionHost &Pipeline::extHost() noexcept { return m_extHost; }

void Pipeline::log(std::string_view msg) const {
  if (m_opts.verbose)
    std::cerr << "[lumora] " << msg << "\n";
}

bool Pipeline::execCmd(const std::string &cmd) const {
  log(std::format("exec: {}", cmd));
  int ret = std::system(cmd.c_str());
  return ret == 0;
}

void Pipeline::ensureOutputDir() const {
  std::filesystem::create_directories(m_cfg.outputDir);
}

std::filesystem::path
Pipeline::outputPath(const std::filesystem::path &src) const {
  return std::filesystem::path(m_cfg.outputDir) / (src.stem().string() + ".ll");
}

void Pipeline::loadExtensions() {
  for (auto &dir : m_cfg.extensionDirs) {
    log(std::format("loading extensions from: {}", dir));
    m_extHost.loadDirectory(dir);
  }
  for (auto &grp : m_cfg.groups) {
    for (auto &dir : grp.extensionDirs) {
      log(std::format("loading group extensions from: {}", dir));
      m_extHost.loadDirectory(dir);
    }
  }
  log(std::format("loaded {} extension(s)", m_extHost.count()));
}

bool Pipeline::run() {
  ensureOutputDir();
  loadExtensions();

  std::vector<std::string> llFiles;
  bool allOk = true;

  for (auto &grp : m_cfg.groups) {
    for (auto &file : grp.files) {
      auto res = compileFile(file);
      if (!res.success) {
        std::cerr << "\x1b[31merror\x1b[0m: compilation failed for " << file
                  << "\n";
        allOk = false;
      } else {
        llFiles.push_back(res.irPath);
      }
    }
  }

  if (!allOk)
    return false;
  if (m_opts.stopAfterIR)
    return true;

  for (auto &optStep : m_cfg.optSteps) {
    if (!runOpt(optStep)) {
      std::cerr << "\x1b[31merror\x1b[0m: opt step failed for " << optStep.input
                << "\n";
      return false;
    }
  }

  for (auto &lnkStep : m_cfg.linkSteps) {
    if (!runLink(lnkStep)) {
      std::cerr << "\x1b[31merror\x1b[0m: link step failed\n";
      return false;
    }
  }

  return true;
}

CompileResult Pipeline::compileFile(const std::filesystem::path &srcPath) {
  CompileResult result;
  result.sourceFile = srcPath.string();

  std::ifstream f(srcPath);
  if (!f) {
    std::cerr << "\x1b[31merror\x1b[0m: cannot open " << srcPath << "\n";
    return result;
  }
  std::ostringstream ss;
  ss << f.rdbuf();
  std::string src = ss.str();

  log(std::format("compiling: {}", srcPath.string()));

  Lexer lex(src, result.sourceFile);

  if (m_opts.dumpTokens) {
    auto tokens = lex.tokenizeAll();
    for (auto &t : tokens)
      std::cout << t.loc.line << ":" << t.loc.col << " " << t.kindName() << " '"
                << t.raw << "'\n";
    result.success = true;
    return result;
  }

  Sema sema;
  IREmitter emitter(sema);
  Parser parser(lex);

  ExtensionAPI api{&lex, &parser, &sema, &emitter};
  m_extHost.setAPI(api);

  auto mod = parser.parseModule(srcPath.string());

  if (parser.hasErrors()) {
    for (auto &e : parser.errors())
      std::cerr << e.loc.file << ":" << e.loc.line << ":" << e.loc.col << ": "
                << e.message << "\n";
    return result;
  }

  if (m_opts.dumpAST) {
    ast::dumpAST(*mod, std::cout);
  }

  sema.analyze(*mod);

  if (sema.hasErrors()) {
    for (auto &e : sema.errors())
      std::cerr << e.loc.file << ":" << e.loc.line << ":" << e.loc.col << ": "
                << e.message << "\n";
    return result;
  }

  result.irOutput = emitter.emit(*mod);
  result.irPath = outputPath(srcPath).string();

  if (m_opts.dumpIR)
    std::cout << result.irOutput;

  std::ofstream out(result.irPath);
  if (!out) {
    std::cerr << "\x1b[31merror\x1b[0m: cannot write " << result.irPath << "\n";
    return result;
  }
  out << result.irOutput;

  result.success = true;
  log(std::format("emitted: {}", result.irPath));
  return result;
}

bool Pipeline::runOpt(const OptStep &step) {
  std::string cmd = "opt";

  if (!step.passes.empty()) {
    std::string passes;
    if (!step.level.empty()) {
      passes = "default<" + step.level + ">";
    }
    for (auto &p : step.passes) {
      if (!passes.empty())
        passes += ",";
      passes += p;
    }
    cmd += " -passes='" + passes + "'";
  } else if (!step.level.empty()) {
    cmd += " -" + step.level;
  }

  for (auto &f : step.extraFlags)
    cmd += " " + f;
  cmd += " " + step.input + " -o " + step.output;
  log(cmd);
  return execCmd(cmd);
}

bool Pipeline::runLink(const LinkStep &step) {
  std::string cmd = step.linker;
  for (auto &f : step.flags)
    cmd += " " + f;
  for (auto &i : step.inputs)
    cmd += " " + i;
  cmd += " -o " + step.output;
  for (auto &l : step.libs)
    cmd += " -l" + l;
  log(cmd);
  return execCmd(cmd);
}

} // namespace lumora
