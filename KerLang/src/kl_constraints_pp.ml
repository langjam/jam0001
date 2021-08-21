
open Kl_constraints

let pp_value oc (value: value) = 
  match value with
  | Arg(id) -> Printf.fprintf oc "x%d" id
  | Cst(value) -> Printf.fprintf oc "%d" value
  | Var(name) -> Printf.fprintf oc "%s" name
  | Hole -> Printf.fprintf oc "%s" "something"

let pp_operation oc (operation: operation) = 
  match operation with
  | If(cond, ifcase, elsecase) -> begin
      Printf.fprintf oc "%s" "(if ";
      pp_value oc cond;
      Printf.fprintf oc "%s" " then ";
      pp_value oc ifcase;
      Printf.fprintf oc "%s" " else ";
      pp_value oc elsecase;
      Printf.fprintf oc "%s" ")"
    end
  | Sum(a, b) -> begin
      Printf.fprintf oc "%s" "(";
      pp_value oc a;
      Printf.fprintf oc "%s" " + ";
      pp_value oc b;
      Printf.fprintf oc "%s" ")"
    end
  | Diff(a, b) -> begin
      Printf.fprintf oc "%s" "(";
      pp_value oc a;
      Printf.fprintf oc "%s" " - ";
      pp_value oc b;
      Printf.fprintf oc "%s" ")"
    end
  | Prod(a, b) -> begin
      Printf.fprintf oc "%s" "(";
      pp_value oc a;
      Printf.fprintf oc "%s" " * ";
      pp_value oc b;
      Printf.fprintf oc "%s" ")"
    end
  | Div(a, b) -> begin
      Printf.fprintf oc "%s" "(";
      pp_value oc a;
      Printf.fprintf oc "%s" " / ";
      pp_value oc b;
      Printf.fprintf oc "%s" ")"
    end
  | App(func_name, value_list) -> begin
      Printf.fprintf oc "(%s" func_name;
      List.iter (fun value -> Printf.fprintf oc "%s" " "; pp_value oc value) value_list;
      Printf.fprintf oc "%s" ")"
    end
  | Rec(value_list) -> begin
      Printf.fprintf oc "%s" "(recursive_call";
      List.iter (fun value -> Printf.fprintf oc "%s" " "; pp_value oc value) value_list;
      Printf.fprintf oc "%s" ")"
    end

let pp_expr oc (expr: expr) = 
  match expr with
  | Leaf(value) -> pp_value oc value
  | Node(operation) -> pp_operation oc operation

let pp_cconstraint oc (cc: cconstraint) =
  match cc with
  | Takes(how_many_args) -> 
    Printf.fprintf oc "- Takes %d arguments\n" how_many_args
  | Let(var_name, expr) -> begin
      Printf.fprintf oc "- Let %s be " var_name;
      pp_expr oc expr
    end
  | Returns(expr) -> begin
      Printf.fprintf oc "%s" "- Returns ";
      pp_expr oc expr
    end
  | Uses(expr_list) -> begin
      Printf.fprintf oc "%s" "- Uses";
      List.iter (fun expr -> Printf.fprintf oc "%s" "\n  * "; pp_expr oc expr) expr_list;
    end
  | Nothing -> 
    Printf.fprintf oc "- Has comments for humans\n"
