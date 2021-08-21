
(** Code generator - To transpile to OCaml.
    Call emit_ast_as_function to print the generated code of a function,
    given its name and its AST. *)

open Kl_IR

let max2 x y = if x > y then x else y
let max3 x y z = if z > x && z > y then z else max2 x y

let rec ast_count_params (func : ast): int =
  match func with
  | Cst _ -> 0
  | Var id -> id + 1 (* The exisence of x3 tells us that there are at least 4 variables. *)
  | App (_, args) ->
    List.fold_right (fun ast acc -> max2 (ast_count_params ast) acc) args 0
  | If (cond, ifcase, elsecase) ->
    max3 (ast_count_params cond) (ast_count_params ifcase) (ast_count_params elsecase)

let op_count_params ?(self_param_count : int = 0) (op : op) : int =
  match op with
  | OUT -> 2
  | ADD -> 2
  | MUL -> 2
  | SUB -> 2
  | FUN (_, func) -> ast_count_params func
  | SELF -> self_param_count

let emit_indent oc (indent_lvl : int) = 
  for _ = 0 to indent_lvl - 1 do
    Printf.fprintf oc "%s" "  "
  done

let emit_param_sequence oc (params_count : int) = 
  for i = 0 to params_count - 2 do
    Printf.fprintf oc "%s" "x";
    print_int i;
    Printf.fprintf oc "%s" " ";
  done;
  if params_count >= 1 then begin
    Printf.fprintf oc "%s" "x";
    print_int (params_count - 1)
  end

let rec emit_ast oc ?(self_name : string option = None) ?(indent_lvl : int = 0) (func: ast) =
  match func with
  | Cst value ->
    Printf.fprintf oc "%s" "\n";
    emit_indent oc indent_lvl;
    print_int value
  | Var id ->
    Printf.fprintf oc "%s" "\n";
    emit_indent oc indent_lvl;
    Printf.fprintf oc "%s" "x";
    print_int id
  | App(op, args) ->
    Printf.fprintf oc "%s" "\n";
    emit_indent oc indent_lvl;
    Printf.fprintf oc "%s" "(";
    emit_op oc ~self_name:(self_name) ~indent_lvl:(indent_lvl) op;
    Printf.fprintf oc "%s" " ";
    emit_ast_list oc ~self_name:(self_name) ~indent_lvl:(indent_lvl + 1) args;
    Printf.fprintf oc "%s" ")"
  | If(cond, ifcase, elsecase) ->
    Printf.fprintf oc "%s" "\n";
    emit_indent oc indent_lvl;
    Printf.fprintf oc "%s" "if 0 <> (";
    emit_ast oc ~self_name:(self_name) ~indent_lvl:(indent_lvl + 1) cond;
    Printf.fprintf oc "%s" ")\n";
    emit_indent oc indent_lvl;
    Printf.fprintf oc "%s" "then";
    emit_ast oc ~self_name:(self_name) ~indent_lvl:(indent_lvl + 1) ifcase;
    Printf.fprintf oc "%s" "\n";
    emit_indent oc indent_lvl;
    Printf.fprintf oc "%s" "else";
    emit_ast oc ~self_name:(self_name) ~indent_lvl:(indent_lvl + 1) elsecase;

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
    Printf.fprintf oc "%s" " -> ";
    Printf.fprintf oc "%s" "print_int x0; print_string \"\\n\"; ";
    Printf.fprintf oc "%s" "x1";
    Printf.fprintf oc "%s" ")"
  | ADD ->
    Printf.fprintf oc "%s" "(+)"
  | SUB ->
    Printf.fprintf oc "%s" "(-)"
  | MUL ->
    Printf.fprintf oc "%s" "( * )"
  | FUN (Some name, _) ->
    Printf.fprintf oc "%s" name
  | FUN (None, func) ->
    Printf.fprintf oc "%s" "(fun ";
    emit_param_sequence oc (op_count_params op);
    Printf.fprintf oc "%s" " -> ";
    emit_ast oc ~self_name:(self_name) ~indent_lvl:(indent_lvl + 1) func;
    Printf.fprintf oc "%s" ")"
  | SELF ->
    match self_name with
    | None -> failwith "were gonna need a self name here"
    | Some name -> Printf.fprintf oc "%s" name

let emit_ast_as_function oc ?(indent_lvl : int = 0) (name : string) (func : ast) = 
  emit_indent oc indent_lvl;
  Printf.fprintf oc "%s" "let rec ";
  Printf.fprintf oc "%s" name;
  Printf.fprintf oc "%s" " = ";
  emit_ast oc ~self_name:(Some name) ~indent_lvl:(indent_lvl + 1) func;
  Printf.fprintf oc "%s" "\n"

let emit_op_as_function oc ?(indent_lvl : int = 0) (op : op) =
  match op with
  | FUN (name_opt, func) -> begin
      match name_opt with
      | Some name -> begin
          emit_indent oc indent_lvl;
          Printf.fprintf oc "%s" "let rec ";
          Printf.fprintf oc "%s" name;
          Printf.fprintf oc "%s" " = ";
          emit_op oc ~self_name:(Some name) ~indent_lvl:(indent_lvl + 1) (FUN (None, func));
          Printf.fprintf oc "%s" "\n"
        end
      | None -> failwith "i aint gonna generate an op with no name"
    end
  | _ -> failwith "whats even that"
