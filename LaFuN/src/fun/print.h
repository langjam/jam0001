#pragma once

#include <iostream>

#include "ast.h"

namespace fun {

void printExpression(std::ostream &os, const ast::Expression &expr, int depth = 0);
void printCodeBlock(std::ostream &os, const ast::CodeBlock &block, int depth = 0);
void printDeclaration(std::ostream &os, const ast::Declaration &decl, int depth = 0);

}
