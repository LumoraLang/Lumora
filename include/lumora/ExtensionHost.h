#pragma once
#include "lumora/Lexer.h"
#include "lumora/Parser.h"
#include "lumora/Sema.h"
#include "lumora/IREmitter.h"
#include <string>
#include <vector>
#include <memory>
#include <functional>
#include <filesystem>

namespace lumora {

struct ExtensionManifest {
    std::string id;
    std::string version;
    std::string description;
    std::vector<std::string> dependencies;
};

struct ExtensionAPI {
    Lexer*     lexer     = nullptr;
    Parser*    parser    = nullptr;
    Sema*      sema      = nullptr;
    IREmitter* emitter   = nullptr;
};

using ExtensionInitFn    = ExtensionManifest(*)(ExtensionAPI&);
using ExtensionDestroyFn = void(*)();

struct LoadedExtension {
    ExtensionManifest manifest;
    void*             handle = nullptr;
    ExtensionDestroyFn destroy = nullptr;
    ExtensionInitFn    init    = nullptr;
};

class ExtensionHost {
public:
    ExtensionHost();
    ~ExtensionHost();

    void setAPI(ExtensionAPI api);

    bool load(const std::filesystem::path& soPath);
    void unloadAll();

    [[nodiscard]] const std::vector<LoadedExtension>& loaded() const noexcept;
    [[nodiscard]] size_t count() const noexcept;

    void loadDirectory(const std::filesystem::path& dir);

private:
    ExtensionAPI                 m_api;
    std::vector<LoadedExtension> m_extensions;
};

} 
