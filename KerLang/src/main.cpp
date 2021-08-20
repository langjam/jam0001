#include "kl_IR.hpp"
#include <iostream>

#if 0
class PrimitiveNode {
  std::string string_;
  std::vector<PrimitiveNode> sub_nodes_;

public:
};
#endif

void parse_function(const std::string &src_code, unsigned int start) {}

int main(int argc, char const *argv[]) {
  std::cout << "Hello KerLang" << std::endl;
  BoxNode A = std::make_shared<IntLit>(8);
  BoxNode B = std::make_shared<IntLit>(34);
  BoxNode C = std::make_shared<IntLit>(1);
  // NodeVec abc = NodeVec({A, B, C});
  BoxNode node = std::make_shared<LazyIfElse>(A, B, C);
  std::cout << node->eval(LocalEnv(), GlobalEnv()) << std::endl;
  node->print();
  return 0;
}
