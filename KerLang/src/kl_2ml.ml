(** OCaml code generator - To transpile to OCaml *)

open Kl_IR

let emit_header oc =
  Printf.fprintf oc "
(*----------------------------------------------------*)
(* This code is generated by the KerLang compiler and *)
(* is not intended to be manually edited              *)
(*----------------------------------------------------*)\n\n"

let emit_indent oc (indent_lvl : int) =
  for _ = 0 to indent_lvl - 1 do
    Printf.fprintf oc "  "
  done

let emit_param_sequence oc (params_count : int) = 
  for i = 0 to params_count - 2 do
    Printf.fprintf oc "x%d " i
  done;
  if params_count >= 1 then begin
    Printf.fprintf oc "x%d" (params_count - 1)
  end

let rec emit_ast oc ?(self_name : string option = None) ?(indent_lvl : int = 0) (func : ast) =
  match func with
  | Cst value ->
    Printf.fprintf oc "\n%a%d" emit_indent indent_lvl value
  | Var id ->
    Printf.fprintf oc "\n%ax%d" emit_indent indent_lvl id
  | App (op, args) ->
    Printf.fprintf oc "\n%a(%a %a)"
      emit_indent indent_lvl
      (emit_op ~self_name) op
      (emit_ast_list ~self_name ~indent_lvl:(indent_lvl + 1)) args
  | If (cond, ifcase, elsecase) ->
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

and emit_op oc ?(self_name : string option = None) (op: op) =
  match op with
  | OUT ->
    Printf.fprintf oc "(fun x0 x1 -> print_int x0; print_string \"\\n\"; x1)";
  | ADD ->
    Printf.fprintf oc "(+)"
  | SUB ->
    Printf.fprintf oc "(-)"
  | MUL ->
    Printf.fprintf oc "( * )"
  | DIV ->
    Printf.fprintf oc "(/)"
  | FUN name ->
    Printf.fprintf oc "%s" name
  | SELF ->
    match self_name with
    | None -> Kl_errors.dev_error "self_name needed but not provided"
    | Some name -> Printf.fprintf oc "%s" name

and emit_ast_as_function oc ?(indent_lvl : int = 0) ?self_name:(self_name = None) (func : ast) =
  let params = ast_count_params func in
  if params > 0 then
    Printf.fprintf oc "(fun %a -> %a)"
      emit_param_sequence params
      (emit_ast ~self_name ~indent_lvl) func
  else
    emit_ast oc ~indent_lvl func

let emit_ast_as_function_decl oc ?(indent_lvl : int = 0) (name : string) (func : ast) =
  emit_indent oc indent_lvl;
  Printf.fprintf oc "let %s%s = %a\n"
    (if ast_is_recursive func then "rec " else "")
    name
    (emit_ast_as_function ~indent_lvl:(indent_lvl + 1) ~self_name:(Some name)) func

let emit_entrypoint_call oc =
  Printf.fprintf oc "\nlet () = main\n"
