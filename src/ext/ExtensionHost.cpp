#include "lumora/ExtensionHost.h"
#include <dlfcn.h>
#include <iostream>
namespace lumora {
ExtensionHost::ExtensionHost() = default;
ExtensionHost::~ExtensionHost() { unloadAll(); }
void ExtensionHost::setAPI(ExtensionAPI api) {
  m_api = api;
  for (auto &ext : m_extensions) {
    if (ext.init)
      ext.init(m_api);
  }
}

bool ExtensionHost::load(const std::filesystem::path &soPath) {
  void *handle = dlopen(soPath.c_str(), RTLD_NOW | RTLD_LOCAL);
  if (!handle) {
    std::cerr << "lumora: extension load failed: " << dlerror() << "\n";
    return false;
  }

  auto initFn = reinterpret_cast<ExtensionInitFn>(dlsym(handle, "lumora_extension_init"));
  if (!initFn) {
    std::cerr << "lumora: extension missing 'lumora_extension_init': " << dlerror() << "\n";
    dlclose(handle);
    return false;
  }

  auto destroyFn = reinterpret_cast<ExtensionDestroyFn>(dlsym(handle, "lumora_extension_destroy"));
  LoadedExtension ext;
  ext.handle = handle;
  ext.destroy = destroyFn;
  ext.init = initFn;
  ext.manifest = initFn(m_api);
  m_extensions.push_back(std::move(ext));
  return true;
}

void ExtensionHost::unloadAll() {
  for (auto &ext : m_extensions) {
    if (ext.destroy) ext.destroy();
    if (ext.handle) dlclose(ext.handle);
  }
  m_extensions.clear();
}

const std::vector<LoadedExtension> &ExtensionHost::loaded() const noexcept {
  return m_extensions;
}
size_t ExtensionHost::count() const noexcept { return m_extensions.size(); }
void ExtensionHost::loadDirectory(const std::filesystem::path &dir) {
  if (!std::filesystem::exists(dir))
    return;
  for (auto &entry : std::filesystem::directory_iterator(dir)) {
    if (entry.path().extension() == ".so")
      load(entry.path());
  }
}
}
