
(** Code generator - To transpile to OCaml.
    Call function_as_ast_generate to print the generated code of a function,
    given its name and its AST. *)

open Kl_IR;;

let max2 x y = if x > y then x else y
let max3 x y z = if z > x && z > y then z else max2 x y

let rec ast_params_count (func: ast): int =
  match func with
  | Cst(_) -> 0
  | Var(id) -> id+1 (* The exisence of x3 tells us that there are at least 4 variables. *)
  | App(_, args) ->
    List.fold_right (fun ast acc -> max2 (ast_params_count ast) acc) args 0
  | If(cond, ifcase, elsecase) ->
    max3 (ast_params_count cond) (ast_params_count ifcase) (ast_params_count elsecase)

let op_params_count ?(self_param_count: int = 0) (op: op): int = 
  match op with
  | OUT -> 2
  | ADD -> 2
  | MUL -> 2
  | SUB -> 2
  | FUN(_, func) -> ast_params_count func
  | SELF -> self_param_count

let print_indent (indent_lvl: int) = 
  for _ = 0 to indent_lvl-1 do
    print_string "  "
  done

let print_params (params_count: int) = 
  for i = 0 to params_count-2 do
    print_string "x";
    print_int i;
    print_string " ";
  done;
  if params_count >= 1 then begin
    print_string "x";
    print_int (params_count-1)
  end

let rec ast_generate ?(self_name: string option = None) ?(indent_lvl: int = 0) (func: ast) =
  match func with
  | Cst(value) -> begin
      print_string "\n";
      print_indent indent_lvl;
      print_int value
    end
  | Var(id) -> begin
      print_string "\n";
      print_indent indent_lvl;
      print_string "x";
      print_int id
    end
  | App(op, args) -> begin
      print_string "\n";
      print_indent indent_lvl;
      print_string "(";
      op_generate ~self_name:(self_name) ~indent_lvl:(indent_lvl) op;
      print_string " ";
      ast_list_generate ~self_name:(self_name) ~indent_lvl:(indent_lvl+1) args;
      print_string ")"
    end
  | If(cond, ifcase, elsecase) -> begin
      print_string "\n";
      print_indent indent_lvl;
      print_string "if 0 <> (";
      ast_generate ~self_name:(self_name) ~indent_lvl:(indent_lvl+1) cond;
      print_string ")\n";
      print_indent indent_lvl;
      print_string "then";
      ast_generate ~self_name:(self_name) ~indent_lvl:(indent_lvl+1) ifcase;
      print_string "\n";
      print_indent indent_lvl;
      print_string "else";
      ast_generate ~self_name:(self_name) ~indent_lvl:(indent_lvl+1) elsecase;
    end

and ast_list_generate ?(self_name: string option = None) ?(indent_lvl: int = 0) (ast_list: ast list) =
  match ast_list with
  | [] -> ()
  | ast::q -> begin
      ast_generate ~self_name:(self_name) ~indent_lvl:(indent_lvl) ast;
      ast_list_generate ~self_name:(self_name) ~indent_lvl:(indent_lvl) q
    end

and op_generate ?(self_name: string option = None) ?(indent_lvl: int = 0) (op: op) =
  match op with
  | OUT -> begin
      print_string "(fun ";
      print_params (op_params_count op);
      print_string " -> ";
      print_string "print_int x0; print_string \"\\n\"; ";
      print_string "x1";
      print_string ")"
    end
  | ADD -> begin
      print_string "(+)"
    end
  | SUB -> begin
      print_string "(-)"
    end
  | MUL -> begin
      print_string "( * )"
    end
  | FUN(Some(name), _) -> begin
      print_string name
    end
  | FUN(None, func) -> begin
      print_string "(fun ";
      print_params (op_params_count op);
      print_string " -> ";
      ast_generate ~self_name:(self_name) ~indent_lvl:(indent_lvl+1) func;
      print_string ")"
    end
  | SELF -> begin
      match self_name with
      | None -> failwith "were gonna need a self name here"
      | Some(name) -> print_string name
    end

let function_as_ast_generate ?(indent_lvl: int = 0) (name: string) (func: ast) = 
  print_indent indent_lvl;
  print_string "let rec ";
  print_string name;
  print_string " = ";
  ast_generate ~self_name:(Some(name)) ~indent_lvl:(indent_lvl+1) func;
  print_string "\n"

let function_as_op_generate ?(indent_lvl: int = 0) (op: op) = 
  match op with
  | FUN(name_opt, func) -> begin
      match name_opt with
      | Some(name) -> begin
          print_indent indent_lvl;
          print_string "let rec ";
          print_string name;
          print_string " = ";
          op_generate ~self_name:(Some(name)) ~indent_lvl:(indent_lvl+1) (FUN(None, func));
          print_string "\n"
        end
      | None -> failwith "i aint gonna generate an op with no name"
    end
  | _ -> failwith "whats even that"
