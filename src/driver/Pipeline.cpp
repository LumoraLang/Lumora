#include "lumora/Pipeline.h"
#include "lumora/ASTDump.h"
#include <cstdlib>
#include <format>
#include <fstream>
#include <iostream>
#include <sstream>
namespace lumora {
Pipeline::Pipeline(LumoraConfig cfg, PipelineOptions opts) : m_cfg(std::move(cfg)), m_opts(std::move(opts)) {}
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
        std::cerr << "\x1b[31merror\x1b[0m: compilation failed for " << file << "\n";
        allOk = false;
      } else {
        llFiles.push_back(res.irPath);
      }
    }
  }

  if (!allOk) return false;
  if (m_opts.multiboot) {
    auto bootPath = std::filesystem::path(m_cfg.outputDir) / "boot.S";
    std::ofstream boot(bootPath);
    if (!boot) {
      std::cerr << "\x1b[31merror\x1b[0m: cannot write " << bootPath << "\n";
      return false;
    }
    boot << R"(.set MB_MAGIC, 0x1BADB002
.set MB_FLAGS, 0x00000000
.set MB_CHECKSUM, -(MB_MAGIC + MB_FLAGS)
.set PML4, 0x80000
.set PDPT, 0x81000
.set PD, 0x82000
.set PT, 0x83000
.set KSTACK, 0x84000
.set KERNEL_ENTRY, 0x101000
.code32
.section .boot, "ax", @progbits
.globl _start32
_start32:
    jmp _init32

.align 4
    .long MB_MAGIC
    .long MB_FLAGS
    .long MB_CHECKSUM

_init32:
    movl %ebx, %edi
    movl %eax, %esi
    movl $KSTACK, %esp
    xorl %ebp, %ebp
    call check_multiboot
    call check_cpuid
    call check_long_mode
    call set_up_page_tables
    call enable_paging
    lgdt gdt64_ptr
    ljmp $0x08, $KERNEL_ENTRY

check_multiboot:
    cmpl $0x2BADB002, %eax
    jne .Lfail_m
    ret
.Lfail_m:
    movl $0x4F450F4D, 0xB8000
    hlt
    jmp .Lfail_m

check_cpuid:
    pushfl
    popl %eax
    movl %eax, %ecx
    xorl $(1 << 21), %eax
    pushl %eax
    popfl
    pushfl
    popl %eax
    pushl %ecx
    popfl
    xorl %ecx, %eax
    jz .Lfail_c
    ret
.Lfail_c:
    movl $0x4F450F43, 0xB8000
    hlt
    jmp .Lfail_c

check_long_mode:
    movl $0x80000000, %eax
    cpuid
    cmpl $0x80000001, %eax
    jb .Lfail_l
    movl $0x80000001, %eax
    cpuid
    testl $(1 << 29), %edx
    jz .Lfail_l
    ret
.Lfail_l:
    movl $0x4F450F4C, 0xB8000
    hlt
    jmp .Lfail_l

set_up_page_tables:
    movl $PDPT, %eax
    orl $3, %eax
    movl %eax, PML4
    movl $PD, %eax
    orl $3, %eax
    movl %eax, PDPT
    movl $PT, %eax
    orl $3, %eax
    movl %eax, PD
    xorl %ecx, %ecx
.Lmap_loop:
    movl $0x1000, %eax
    mull %ecx
    orl $3, %eax
    movl %eax, PT(,%ecx,8)
    incl %ecx
    cmpl $512, %ecx
    jne .Lmap_loop
    ret

enable_paging:
    movl $PML4, %eax
    movl %eax, %cr3
    movl %cr4, %eax
    orl $(1 << 5), %eax
    movl %eax, %cr4
    movl $0xC0000080, %ecx
    rdmsr
    orl $(1 << 8), %eax
    wrmsr
    movl %cr0, %eax
    orl $(1 << 31), %eax
    orl $1, %eax
    movl %eax, %cr0
    ret

.align 16
gdt64:
    .quad 0
gdt64_code = . - gdt64
    .quad 0x00AF9A000000FFFF
gdt64_data = . - gdt64
    .quad 0x00CF92000000FFFF
gdt64_ptr:
    .short . - gdt64 - 1
    .long gdt64

.code64
.globl memcpy
memcpy:
    pushq %rbp
    movq %rsp, %rbp
    pushq %rdi
    movq %rdi, %rax
    movq %rdx, %rcx
    cld
    rep movsb
    popq %rax
    popq %rbp
    ret

.section .note.GNU-stack, "", @progbits
)";
    boot.close();
    log(std::format("wrote: {}", bootPath.string()));
  }

  if (m_opts.stopAfterIR)return true;
  for (auto &optStep : m_cfg.optSteps) {
    if (!runOpt(optStep)) {
      std::cerr << "\x1b[31merror\x1b[0m: opt step failed for " << optStep.input << "\n";
      return false;
    }
  }

  for (auto &lnkStep : m_cfg.linkSteps) {
    if (!runLink(lnkStep)) {
      std::cerr << "\x1b[31merror\x1b[0m: link step failed\n";
      return false;
    }
  }

  for (auto &cmdStep : m_cfg.commandSteps) {
    if (!runCommand(cmdStep)) {
      std::cerr << "\x1b[31merror\x1b[0m: command step failed\n";
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
    for (auto &t : tokens) std::cout << t.loc.line << ":" << t.loc.col << " " << t.kindName() << " '" << t.raw << "'\n";
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
      std::cerr << e.loc.file << ":" << e.loc.line << ":" << e.loc.col << ": " << e.message << "\n";
    return result;
  }

  if (m_opts.dumpAST) {
    ast::dumpAST(*mod, std::cout);
  }

  sema.analyze(*mod);
  if (sema.hasErrors()) {
    for (auto &e : sema.errors()) std::cerr << e.loc.file << ":" << e.loc.line << ":" << e.loc.col << ": " << e.message << "\n";
    return result;
  }

  result.irOutput = emitter.emit(*mod);
  result.irPath = outputPath(srcPath).string();
  if (m_opts.dumpIR) std::cout << result.irOutput;
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
  std::string cmd = step.program;
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

  for (auto &f : step.extraFlags) cmd += " " + f;
  cmd += " " + step.input + " -o " + step.output;
  log(cmd);
  return execCmd(cmd);
}

bool Pipeline::runLink(const LinkStep &step) {
  std::string cmd = step.linker;
  if (!step.script.empty())
    cmd += " -T " + step.script;
  for (auto &f : step.flags)
    cmd += " " + f;
  for (auto &i : step.inputs)
    cmd += " " + i;
  if (!step.output.empty())
    cmd += " -o " + step.output;
  for (auto &l : step.libs)
    cmd += " -l" + l;
  log(cmd);
  return execCmd(cmd);
}

bool Pipeline::runCommand(const CommandStep &step) {
  log(step.cmd);
  return execCmd(step.cmd);
}
}
