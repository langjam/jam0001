#pragma once

#include "ast.h"

namespace lafun {

void codegen(std::ostream &os, std::string_view source, const ast::LafunDocument &doc);

}
