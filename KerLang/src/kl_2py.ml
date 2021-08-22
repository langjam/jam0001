
(** Python3 code generator - To transpile to Python3.
    Call emit_ast_as_function to print the generated code of a function,
    given its name and its AST. *)

open Kl_IR

let emit_header oc =
  Printf.fprintf oc "
r\"\"\"-------------------------------------------------------\\
| This code is generated by the KerLang compiler and      |
| is not intended to be manually edited                   |
\\--------------------------------------------------------\"\"\"

def out(x0, x1):
    print(x0)
    return x1

"

let emit_indent oc (indent_lvl : int) =
  for _ = 0 to indent_lvl - 1 do
    Printf.fprintf oc "    "
  done

let emit_param_sequence oc (params_count : int) = 
  for i = 0 to params_count - 2 do
    Printf.fprintf oc "x%d, " i
  done;
  if params_count >= 1 then begin
    Printf.fprintf oc "x%d" (params_count - 1)
  end

let rec emit_ast oc ?(self_name : string option = None) ?(indent_lvl : int = 0) (func: ast) =
  match func with
  | Cst value ->
    Printf.fprintf oc "%d" value
  | Var id ->
    Printf.fprintf oc "x%d" id
  | App(op, args) ->
    Printf.fprintf oc "%a(%a)"
      (emit_op ~self_name ~indent_lvl) op
      (emit_ast_list ~self_name ~indent_lvl:(indent_lvl + 1)) args
  | If(cond, ifcase, elsecase) ->
    Printf.fprintf oc "(%a if %a else %a)"
      (emit_ast ~self_name ~indent_lvl:(indent_lvl + 1)) ifcase
      (emit_ast ~self_name ~indent_lvl:(indent_lvl + 1)) cond
      (emit_ast ~self_name ~indent_lvl:(indent_lvl + 1)) elsecase

and emit_ast_list oc ?(self_name : string option = None) ?(indent_lvl : int = 0) (ast_list : ast list) =
  match ast_list with
  | [] -> ()
  | ast::[] ->
    emit_ast oc ~self_name:(self_name) ~indent_lvl:(indent_lvl) ast
  | ast::q ->
    emit_ast oc ~self_name:(self_name) ~indent_lvl:(indent_lvl) ast;
    Printf.fprintf oc "%s" ", ";
    emit_ast_list oc ~self_name:(self_name) ~indent_lvl:(indent_lvl) q

and emit_op oc ?(self_name : string option = None) ?(indent_lvl : int = 0) (op: op) =
  match op with
  | OUT ->
    Printf.fprintf oc "%s" "out";
  | ADD ->
    Printf.fprintf oc "(lambda x0, x1: x0 + x1)"
  | SUB ->
    Printf.fprintf oc "(lambda x0, x1: x0 - x1)"
  | MUL ->
    Printf.fprintf oc "(lambda x0, x1: x0 * x1)"
  | DIV ->
    Printf.fprintf oc "(lambda x0, x1: x0 / x1)"
  | FUN (Some name, _) ->
    Printf.fprintf oc "%s" name
  | FUN (None, func) ->
    let params_count = op_count_params op in
    if params_count = 0 then
      emit_ast oc ~self_name ~indent_lvl:(indent_lvl + 1) func
    else
      Printf.fprintf oc "(lambda %a: %a)"
        emit_param_sequence params_count
        (emit_ast ~self_name ~indent_lvl:(indent_lvl + 1)) func
  | SELF ->
    match self_name with
    | None -> failwith "were gonna need a self name here"
    | Some name -> Printf.fprintf oc "%s" name

let emit_ast_as_function oc ?(indent_lvl : int = 0) (name : string) (func : ast) = 
  emit_indent oc indent_lvl;
  Printf.fprintf oc "%s = %a\n"
    name
    (emit_op ~self_name:(Some name) ~indent_lvl:(indent_lvl + 1)) (FUN (None, func))

let emit_op_as_function oc ?(indent_lvl : int = 0) (op : op) =
  match op with
  | FUN (name_opt, func) -> begin
      match name_opt with
      | Some name -> begin
          Printf.fprintf oc "%s = %a\n"
            name
            (emit_op ~self_name:(Some name) ~indent_lvl:(indent_lvl + 1)) (FUN (None, func))
        end
      | None -> failwith "i aint gonna generate an op with no name"
    end
  | _ -> failwith "whats even that"
