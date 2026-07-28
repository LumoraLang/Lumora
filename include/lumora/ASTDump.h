#pragma once
#include "lumora/AST.h"
#include <iosfwd>
namespace lumora::ast {
void dumpAST(const Node& root, std::ostream& out);
}
