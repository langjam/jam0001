#pragma once

#include <memory>
#include <string_view>
#include <vector>

struct Statement;

void Interpret(std::string_view filePrefix, std::vector<std::unique_ptr<Statement>>& statements);
