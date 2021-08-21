
open Kl_expr

let pp_value oc (value: value) = 
  match value with
  | ARG(id) -> Printf.fprintf oc "x%d" id
  | CONST(value) -> Printf.fprintf oc "%d" value
  | VAR(name) -> Printf.fprintf oc "%s" name
  | SOMETHING -> Printf.fprintf oc "%s" "something"

let pp_value_operation oc (vo: value operation) = 
  match vo with
  | IF(cond, ifcase, elsecase) -> begin
      Printf.fprintf oc "%s" "(if ";
      pp_value oc cond;
      Printf.fprintf oc "%s" " then ";
      pp_value oc ifcase;
      Printf.fprintf oc "%s" " else ";
      pp_value oc elsecase;
      Printf.fprintf oc "%s" ")"
    end
  | SUM(a, b) -> begin
      Printf.fprintf oc "%s" "(";
      pp_value oc a;
      Printf.fprintf oc "%s" " + ";
      pp_value oc b;
      Printf.fprintf oc "%s" ")"
    end
  | DIFF(a, b) -> begin
      Printf.fprintf oc "%s" "(";
      pp_value oc a;
      Printf.fprintf oc "%s" " - ";
      pp_value oc b;
      Printf.fprintf oc "%s" ")"
    end
  | PROD(a, b) -> begin
      Printf.fprintf oc "%s" "(";
      pp_value oc a;
      Printf.fprintf oc "%s" " * ";
      pp_value oc b;
      Printf.fprintf oc "%s" ")"
    end
  | DIV(a, b) -> begin
      Printf.fprintf oc "%s" "(";
      pp_value oc a;
      Printf.fprintf oc "%s" " / ";
      pp_value oc b;
      Printf.fprintf oc "%s" ")"
    end
  | APPLY(func_name, value_list) -> begin
      Printf.fprintf oc "(%s" func_name;
      List.iter (fun value -> Printf.fprintf oc "%s" " "; pp_value oc value) value_list;
      Printf.fprintf oc "%s" ")"
    end
  | SELF(value_list) -> begin
      Printf.fprintf oc "%s" "(recursive_call";
      List.iter (fun value -> Printf.fprintf oc "%s" " "; pp_value oc value) value_list;
      Printf.fprintf oc "%s" ")"
    end

let pp_expr oc (expr: expr) = 
  match expr with
  | Leaf(value) -> pp_value oc value
  | Node(value_operation) -> pp_value_operation oc value_operation

let pp_expr_statement oc (es: expr statement) =
  match es with
  | TAKES(how_many_args) -> 
    Printf.fprintf oc "- Takes %d arguments\n" how_many_args
  | LET(var_name, expr) -> begin
      Printf.fprintf oc "- Let %s be " var_name;
      pp_expr oc expr
    end
  | RETURNS(expr) -> begin
      Printf.fprintf oc "%s" "- Returns ";
      pp_expr oc expr
    end
  | USES(expr_list) -> begin
      Printf.fprintf oc "%s" "- Uses";
      List.iter (fun expr -> Printf.fprintf oc "%s" "\n  * "; pp_expr oc expr) expr_list;
    end
  | NONE -> 
    Printf.fprintf oc "- Has comments for humans\n"
