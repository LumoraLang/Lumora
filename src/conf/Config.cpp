#include "lumora/Config.h"
#include <format>
#include <fstream>
#include <sstream>
#include <stdexcept>
namespace lumora {
namespace {
struct ConfigParser {
  std::string src;
  size_t pos = 0;
  uint32_t line = 1;
  LumoraConfig cfg;
  std::string currentSection;
  std::string currentSubSection;
  void skip() {
    while (pos < src.size()) {
      if (src[pos] == '#') {
        while (pos < src.size() && src[pos] != '\n')
          pos++;
        continue;
      }
      if (src[pos] == ' ' || src[pos] == '\t' || src[pos] == '\r') {
        pos++;
        continue;
      }
      if (src[pos] == '\n') {
        pos++;
        line++;
        continue;
      }
      break;
    }
  }

  bool atEnd() const { return pos >= src.size(); }
  std::string readLine() {
    std::string out;
    while (pos < src.size() && src[pos] != '\n') {
      if (src[pos] != '\r') out += src[pos];
      pos++;
    }
    if (pos < src.size()) {
      pos++;
      line++;
    }
    return out;
  }

  std::string trim(std::string_view sv) {
    size_t s = sv.find_first_not_of(" \t");
    if (s == std::string_view::npos) return "";
    size_t e = sv.find_last_not_of(" \t");
    return std::string(sv.substr(s, e - s + 1));
  }

  std::string stripQuotes(std::string_view sv) {
    if (sv.size() >= 2 && sv.front() == '"' && sv.back() == '"') return std::string(sv.substr(1, sv.size() - 2));
    return std::string(sv);
  }

  std::vector<std::string> parseArray(std::string_view sv) {
    std::vector<std::string> result;
    sv = sv.substr(sv.find('[') + 1);
    if (auto end = sv.rfind(']'); end != std::string_view::npos) sv = sv.substr(0, end);
    std::stringstream ss{std::string(sv)};
    std::string item;
    while (std::getline(ss, item, ',')) {
      auto t = trim(stripQuotes(trim(item)));
      if (!t.empty()) result.push_back(t);
    }
    return result;
  }

  void parseSection(std::string_view header) {
    auto inner = header.substr(1, header.size() - 2);
    auto dot = inner.find('.');
    if (dot != std::string_view::npos) {
      currentSection = std::string(inner.substr(0, dot));
      currentSubSection = std::string(inner.substr(dot + 1));
    } else {
      currentSection = std::string(inner);
      currentSubSection = "";
    }

    if (currentSection == "source") {
      cfg.groups.push_back({currentSubSection.empty() ? "default" : currentSubSection, {}, {}});
    } else if (currentSection == "opt") {
      cfg.optSteps.push_back({});
    } else if (currentSection == "link") {
      cfg.linkSteps.push_back({});
    } else if (currentSection == "command") {
      cfg.commandSteps.push_back({});
    }
  }

  void applyKeyVal(std::string_view key, std::string_view rawVal) {
    auto val = stripQuotes(trim(rawVal));
    if (currentSection.empty()) {
      if (key == "name")
        cfg.name = val;
      else if (key == "version")
        cfg.version = val;
      else if (key == "output_dir")
        cfg.outputDir = val;
      else if (key == "multiboot")
        cfg.multiboot = (val == "true" || val == "1");
      else
        cfg.vars[std::string(key)] = val;
      return;
    }

    if (currentSection == "extensions") {
      if (rawVal.find('[') != std::string_view::npos) {
        auto dirs = parseArray(rawVal);
        cfg.extensionDirs.insert(cfg.extensionDirs.end(), dirs.begin(), dirs.end());
      } else if (key == "dir") {
        cfg.extensionDirs.push_back(val);
      }
      return;
    }

    if (currentSection == "source") {
      if (cfg.groups.empty()) return;
      auto &grp = cfg.groups.back();
      if (key == "files" && rawVal.find('[') != std::string_view::npos) {
        auto files = parseArray(rawVal);
        grp.files.insert(grp.files.end(), files.begin(), files.end());
      } else if (key == "file") {
        grp.files.push_back(val);
      } else if (key == "extensions" && rawVal.find('[') != std::string_view::npos) {
        auto dirs = parseArray(rawVal);
        grp.extensionDirs.insert(grp.extensionDirs.end(), dirs.begin(), dirs.end());
      }
      return;
    }

    if (currentSection == "opt") {
      if (cfg.optSteps.empty()) return;
      auto &opt = cfg.optSteps.back();
      if (key == "input")
        opt.input = val;
      else if (key == "output")
        opt.output = val;
      else if (key == "program")
        opt.program = val;
      else if (key == "level")
        opt.level = val;
      else if (key == "passes" && rawVal.find('[') != std::string_view::npos)
        opt.passes = parseArray(rawVal);
      else if (key == "flags" && rawVal.find('[') != std::string_view::npos)
        opt.extraFlags = parseArray(rawVal);
      return;
    }

    if (currentSection == "link") {
      if (cfg.linkSteps.empty())
        return;
      auto &lnk = cfg.linkSteps.back();
      if (key == "output")
        lnk.output = val;
      else if (key == "linker")
        lnk.linker = val;
      else if (key == "script")
        lnk.script = val;
      else if (key == "inputs" && rawVal.find('[') != std::string_view::npos)
        lnk.inputs = parseArray(rawVal);
      else if (key == "libs" && rawVal.find('[') != std::string_view::npos)
        lnk.libs = parseArray(rawVal);
      else if (key == "flags" && rawVal.find('[') != std::string_view::npos)
        lnk.flags = parseArray(rawVal);
      return;
    }

    if (currentSection == "command") {
      if (cfg.commandSteps.empty())
        return;
      auto &cmd = cfg.commandSteps.back();
      if (key == "cmd")
        cmd.cmd = val;
      return;
    }
  }

  LumoraConfig parse() {
    while (!atEnd()) {
      skip();
      if (atEnd()) break;
      auto rawLine = readLine();
      auto line_ = trim(rawLine);
      if (line_.empty() || line_.front() == '#')
        continue;

      if (line_.front() == '[') {
        parseSection(line_);
        continue;
      }

      auto eq = line_.find('=');
      if (eq == std::string::npos) continue;
      auto key = trim(line_.substr(0, eq));
      auto val = trim(line_.substr(eq + 1));
      applyKeyVal(key, val);
    }
    return cfg;
  }
};
}

LumoraConfig LumoraConfig::loadFromFile(const std::filesystem::path &path) {
  std::ifstream f(path);
  if (!f) throw std::runtime_error(std::format("cannot open config: {}", path.string()));
  std::ostringstream ss;
  ss << f.rdbuf();
  return loadFromString(ss.str());
}

LumoraConfig LumoraConfig::loadFromString(std::string_view content) {
  ConfigParser p;
  p.src = std::string(content);
  return p.parse();
}

std::string LumoraConfig::resolve(std::string_view val) const {
  std::string result(val);
  for (auto &[k, v] : vars) {
    std::string pat = "${" + k + "}";
    size_t pos = 0;
    while ((pos = result.find(pat, pos)) != std::string::npos) {
      result.replace(pos, pat.size(), v);
      pos += v.size();
    }
  }
  return result;
}
}
