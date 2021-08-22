open Kl_constraints

let rec pp_expr oc (expr: expr) =
  match expr with
  | Leaf value -> pp_value oc value
  | Node operation -> pp_operation oc operation

and pp_value oc (value: value) =
  match value with
  | Arg id -> Printf.fprintf oc "x%d" id
  | Cst value -> Printf.fprintf oc "%d" value
  | Var name -> Printf.fprintf oc "%s" name
  | Hole -> Printf.fprintf oc "{??}"

and pp_operation oc (operation : operation) =
  match operation with
  | If (cond, ifcase, elsecase) ->
    Printf.fprintf oc "(if %a then %a else %a)"
      pp_expr cond
      pp_expr ifcase
      pp_expr elsecase
  | Sum (a, b) ->
    Printf.fprintf oc "(%a + %a)" pp_expr a pp_expr b
  | Diff (a, b) ->
    Printf.fprintf oc "(%a - %a)" pp_expr a pp_expr b
  | Prod (a, b) ->
    Printf.fprintf oc "(%a * %a)" pp_expr a pp_expr b
  | Div (a, b) ->
    Printf.fprintf oc "(%a / %a)" pp_expr a pp_expr b
  | App (func_name, value_list) ->
    Printf.fprintf oc "(%s " func_name;
    List.iter (fun value -> Printf.fprintf oc "%a " pp_expr value) value_list;
    Printf.fprintf oc ")"
  | Rec value_list ->
    Printf.fprintf oc "(rec ";
    List.iter (fun value -> Printf.fprintf oc "%a " pp_expr value) value_list;
    Printf.fprintf oc ")"

let pp_cconstraint oc (cc: cconstraint) =
  match cc with
  | Takes how_many_args ->
    Printf.fprintf oc "- Takes %d arguments\n" how_many_args
  | Let (var_name, expr) ->
    Printf.fprintf oc "- Let %s be %a" var_name pp_expr expr
  | Returns expr ->
    Printf.fprintf oc "- Returns %a" pp_expr expr
  | Uses expr_list ->
    Printf.fprintf oc "- Uses";
    List.iter (fun expr -> Printf.fprintf oc "%s" "\n  * "; pp_expr oc expr) expr_list
  | Nothing ->
    Printf.fprintf oc "- Has comments for humans\n"
