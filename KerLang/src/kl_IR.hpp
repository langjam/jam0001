#ifndef KL_IR_HPP
#define KL_IR_HPP

#include <cassert>
#include <iostream>
#include <memory>
#include <string>
#include <vector>

class Node;
typedef std::shared_ptr<Node> BoxNode;
typedef std::vector<BoxNode> NodeVec;

class Function;

/* Contains the global state of the current program, i.e. the functions. */
class GlobalEnv {
  std::vector<Function> functions_;

public:
  GlobalEnv() {}
  void add_function(const Function &function) {
    functions_.push_back(function);
  }
  const Function &operator[](int arg_index) const {
    return functions_[arg_index];
  }
  const Function &get_id_from_name(const std::string &function_name) const {
    for (const Function &function : functions_) {
      if (functions_[i].name_)
    }
  }
};

/* Contains the values of the current function arguments. */
class LocalEnv {
  std::vector<int> arg_values_;

public:
  LocalEnv() {}
  LocalEnv(std::vector<int> arg_values) : arg_values_(arg_values) {}

  int operator[](int arg_index) const { return arg_values_[arg_index]; }
};

class Node {
public:
  virtual int eval(const LocalEnv &local_env,
                   const GlobalEnv &global_env) const = 0;
  virtual void print() const = 0;
};

class Function {
  std::string name_;
  unsigned int parameter_count_;
  BoxNode ast_;

public:
  Function(const std::string &name, unsigned int parameter_count, BoxNode ast)
      : name_(name), parameter_count_(parameter_count), ast_(ast) {}
  unsigned int get_parameter_count() const { return parameter_count_; }
  int call(const LocalEnv &local_env, const GlobalEnv &global_env) const {
    return ast_->eval(local_env, global_env);
  }
};

class Arg : public Node {
private:
  int index_;

public:
  Arg(int index) : index_(index){};
  int eval(const LocalEnv &local_env,
           const GlobalEnv &global_env) const override {
    return local_env[index_];
  }
  void print() const override { std::cout << "x" << index_; }
};

class IntLit : public Node {
private:
  int value_;

public:
  IntLit(int value) : value_(value){};
  int eval(const LocalEnv &local_env,
           const GlobalEnv &global_env) const override {
    return value_;
  }
  void print() const override { std::cout << value_; }
};

class Arith : public Node {
private:
  std::string function_name_;
  NodeVec args_;

public:
  Arith(std::string function_name, NodeVec args)
      : function_name_(function_name), args_(args){};
  int eval(const LocalEnv &local_env,
           const GlobalEnv &global_env) const override {
    const Function &function = global_env[function_id_];
    unsigned int parameter_count = function.get_parameter_count();
    assert(parameter_count == args_.size());

    std::vector<int> args(parameter_count);
    for (unsigned int i = 0; i < parameter_count; i++) {
      args[i] = args_[i]->eval(local_env, global_env);
    }

    return function.call(LocalEnv(args), global_env);
  }
  void print() const override {
    std::cout << "(" << function_name_ << " ";
    bool is_first = true;
    for (auto &&arg : args_) {
      if (is_first) {
        is_first = false;
      } else {
        std::cout << ", ";
      }
      arg->print();
    }
    std::cout << ")";
  }
};

class LazyIfElse : public Node {
private:
  BoxNode condition_;
  BoxNode then_node_;
  BoxNode else_node_;

public:
  LazyIfElse(BoxNode condition, BoxNode then_node, BoxNode else_node)
      : condition_(condition), then_node_(then_node), else_node_(else_node) {}

  int eval(const LocalEnv &local_env,
           const GlobalEnv &global_env) const override {
    int condition_value = condition_->eval(local_env, global_env);
    if (condition_value != 0) {
      return then_node_->eval(local_env, global_env);
    } else {
      return else_node_->eval(local_env, global_env);
    }
  }

  void print() const override {
    std::cout << "("
              << "ifelse"
              << " ";
    condition_->print();
    std::cout << " ";
    then_node_->print();
    std::cout << " ";
    else_node_->print();
    std::cout << ")";
  }
};

#endif
