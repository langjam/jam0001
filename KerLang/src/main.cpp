#include <iostream>
#include <memory>
#include <vector>

class Node {};

class Sym : public Node {
private:
  std::string val_;

public:
  Sym(std::string val) : val_(val){};
};

class IntLit : public Node {
private:
  int val_;

public:
  IntLit(int i) : val_(i){};
};

typedef std::shared_ptr<Node> BoxNode;
typedef std::vector<BoxNode> NodeVec;

class ListLit : public Node {
  NodeVec nodes_;

public:
  ListLit(NodeVec nodes) : nodes_(nodes){};
};

enum Op { ADD, MUL, SUB, DIV };

class Arith : public Node {
private:
  Op op_;
  ListLit args_;

public:
  Arith(Op op, ListLit args) : op_(op), args_(args){};
  Arith(Op op, NodeVec args) : op_(op), args_(ListLit(args)){};
};

int main(int argc, char const *argv[]) {
  std::cout << "Hello KerLang" << std::endl;
  BoxNode A = std::make_shared<Sym>("a");
  BoxNode B = std::make_shared<Sym>("b");
  BoxNode C = std::make_shared<IntLit>(1);
  NodeVec prog = NodeVec({A, B, C});
  return 0;
}
