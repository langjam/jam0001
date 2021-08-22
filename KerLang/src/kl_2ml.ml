
(** Code generator - To transpile to OCaml.
    Call emit_ast_as_function to print the generated code of a function,
    given its name and its AST. *)

open Kl_IR

let max2 x y = if x > y then x else y
let max3 x y z = if z > x && z > y then z else max2 x y



let op_count_params ?(self_param_count : int = 0) (op : op) : int =
  match op with
  | OUT -> 2
  | ADD -> 2
  | MUL -> 2
  | DIV -> 2
  | SUB -> 2
  | FUN (_, func) -> ast_count_params func
  | SELF -> self_param_count

let rec ast_is_recursive (func : ast): bool =
  let op_is_self (op: op): bool = 
    match op with
    | SELF -> true
    | _ -> false
  in
  match func with
  | Cst _ -> false
  | Var _ -> false
  | App (op, args) ->
    if (op_is_self op) then
      true
    else
      List.fold_right (fun ast acc -> (||) (ast_is_recursive ast) acc) args false
  | If (cond, ifcase, elsecase) ->
    (ast_is_recursive cond) || (ast_is_recursive ifcase) || (ast_is_recursive elsecase)

let emit_indent oc (indent_lvl : int) = 
  for _ = 0 to indent_lvl - 1 do
    Printf.fprintf oc "%s" "  "
  done

let emit_param_sequence oc (params_count : int) = 
  for i = 0 to params_count - 2 do
    Printf.fprintf oc "x%d " i
  done;
  if params_count >= 1 then begin
    Printf.fprintf oc "x%d" (params_count - 1)
  end

let rec emit_ast oc ?(self_name : string option = None) ?(indent_lvl : int = 0) (func: ast) =
  match func with
  | Cst value ->
    Printf.fprintf oc "\n%a%d" emit_indent indent_lvl value
  | Var id ->
    Printf.fprintf oc "\n%ax%d" emit_indent indent_lvl id
  | App(op, args) ->
    Printf.fprintf oc "\n%a(%a %a)"
      emit_indent indent_lvl
      (emit_op ~self_name ~indent_lvl) op
      (emit_ast_list ~self_name ~indent_lvl:(indent_lvl + 1)) args
  | If(cond, ifcase, elsecase) ->
    Printf.fprintf oc "\n%aif 0 <> (%a)\n%athen%a\n%aelse%a"
      emit_indent indent_lvl
      (emit_ast ~self_name ~indent_lvl:(indent_lvl + 1)) cond
      emit_indent indent_lvl
      (emit_ast ~self_name ~indent_lvl:(indent_lvl + 1)) ifcase
      emit_indent indent_lvl
      (emit_ast ~self_name ~indent_lvl:(indent_lvl + 1)) elsecase

and emit_ast_list oc ?(self_name : string option = None) ?(indent_lvl : int = 0) (ast_list : ast list) =
  match ast_list with
  | [] -> ()
  | ast::q ->
    emit_ast oc ~self_name:(self_name) ~indent_lvl:(indent_lvl) ast;
    emit_ast_list oc ~self_name:(self_name) ~indent_lvl:(indent_lvl) q

and emit_op oc ?(self_name : string option = None) ?(indent_lvl : int = 0) (op: op) =
  match op with
  | OUT ->
    Printf.fprintf oc "%s" "(fun ";
    emit_param_sequence oc (op_count_params op);
    Printf.fprintf oc " -> ";
    Printf.fprintf oc "print_int x0; print_string \"\\n\"; ";
    Printf.fprintf oc "x1)";
  | ADD ->
    Printf.fprintf oc "(+)"
  | SUB ->
    Printf.fprintf oc "(-)"
  | MUL ->
    Printf.fprintf oc "( * )"
  | DIV ->
    Printf.fprintf oc "(/)"
  | FUN (Some name, _) ->
    Printf.fprintf oc "%s" name
  | FUN (None, func) ->
    let params = op_count_params op in
    if params = 0 then
      emit_ast oc ~self_name ~indent_lvl:(indent_lvl + 1) func
    else
      Printf.fprintf oc "(fun %a -> %a)"
        emit_param_sequence params
        (emit_ast ~self_name ~indent_lvl:(indent_lvl + 1)) func
  | SELF ->
    match self_name with
    | None -> failwith "were gonna need a self name here"
    | Some name -> Printf.fprintf oc "%s" name

let emit_ast_as_function oc ?(indent_lvl : int = 0) (name : string) (func : ast) = 
  emit_indent oc indent_lvl;
  Printf.fprintf oc "let %s%s = %a\n"
  (if ast_is_recursive func then "rec " else "")
  name
  (emit_op ~self_name:(Some name) ~indent_lvl:(indent_lvl + 1)) (FUN (None, func))

let emit_op_as_function oc ?(indent_lvl : int = 0) (op : op) =
  match op with
  | FUN (name_opt, func) -> begin
      match name_opt with
      | Some name -> begin
        Printf.fprintf oc "%alet %s %s = %a\n"
        emit_indent indent_lvl
        (if ast_is_recursive func then "rec" else "")
        name
        (emit_op ~self_name:(Some name) ~indent_lvl:(indent_lvl + 1)) (FUN (None, func))
        end
      | None -> failwith "i aint gonna generate an op with no name"
    end
  | _ -> failwith "whats even that"
