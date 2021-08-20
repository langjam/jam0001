#include "kl_IR.hpp"
#include <iostream>

int main(int argc, char const *argv[]) {
  std::cout << "Hello KerLang" << std::endl;
  BoxNode A = std::make_shared<Sym>("a");
  BoxNode B = std::make_shared<Sym>("b");
  BoxNode C = std::make_shared<IntLit>(1);
  NodeVec prog = NodeVec({A, B, C});
  return 0;
}
