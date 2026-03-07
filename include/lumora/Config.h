#pragma once
#include <string>
#include <vector>
#include <unordered_map>
#include <filesystem>
#include <optional>

namespace lumora {

struct LinkStep {
    std::vector<std::string> inputs;
    std::string              output;
    std::string              linker = "clang";
    std::vector<std::string> flags;
    std::vector<std::string> libs;
};

struct OptStep {
    std::string              input;
    std::string              output;
    std::string              level = "O2";
    std::vector<std::string> passes;
    std::vector<std::string> extraFlags;
};

struct SourceGroup {
    std::string              name;
    std::vector<std::string> files;
    std::vector<std::string> extensionDirs;
};

struct LumoraConfig {
    std::string              name;
    std::string              version;
    std::string              outputDir = "build";
    std::vector<SourceGroup> groups;
    std::vector<OptStep>     optSteps;
    std::vector<LinkStep>    linkSteps;
    std::vector<std::string> extensionDirs;
    std::vector<std::string> includeDirs;
    std::vector<std::string> defines;
    std::unordered_map<std::string, std::string> vars;

    static LumoraConfig loadFromFile(const std::filesystem::path& path);
    static LumoraConfig loadFromString(std::string_view content);

    [[nodiscard]] std::string resolve(std::string_view val) const;
};

} 
