
(** Code generator - To transpile to OCaml. *)

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

let op_params_count ?(self_param_count=0) (op: op): int = 
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
  print_string "x";
  print_int (params_count-1)

let rec ast_generate ?(indent_lvl=0) (func: ast) =
  match func with
  | Cst(value) -> begin
      print_indent indent_lvl;
      print_int value;
      print_string "\n"
    end
  | Var(id) -> begin
      print_indent indent_lvl;
      print_string "x";
      print_int id;
      print_string "\n"
    end
  | App(op, _) -> begin
      print_indent indent_lvl;
      print_string "(";
      op_generate ~indent_lvl:(indent_lvl+1) op;
      print_string "\n";
      (* TODO *)
      print_string "\nTODO: args\n";
      print_string ")"
    end
  | If(cond, ifcase, elsecase) -> begin
      print_indent indent_lvl;
      print_string "if\n";
      ast_generate ~indent_lvl:(indent_lvl+1) cond;
      print_string "\n";
      print_indent indent_lvl;
      print_string "then\n";
      ast_generate ~indent_lvl:(indent_lvl+1) ifcase;
      print_string "\n";
      print_indent indent_lvl;
      print_string "else\n";
      ast_generate ~indent_lvl:(indent_lvl+1) elsecase;
      print_string "\n"
    end

and op_generate ?(indent_lvl=0) (op: op) =
  match op with
  | OUT -> begin
      print_indent indent_lvl;
      print_string "(fun ";
      print_params (op_params_count op);
      print_string " -> ";
      print_string "\n";
      print_indent (indent_lvl+1);
      print_string "print_int x0;\n";
      print_indent (indent_lvl+1);
      print_string "x1\n";
      print_indent indent_lvl;
      print_string ")"
    end
  | ADD -> begin
      print_indent indent_lvl;
      print_string "(fun ";
      print_params (op_params_count op);
      print_string " -> ";
      print_string "\n";
      print_indent (indent_lvl+1);
      print_string "x0 + x1\n";
      print_indent indent_lvl;
      print_string ")"
    end
  | MUL -> begin
      print_indent indent_lvl;
      print_string "(fun ";
      print_params (op_params_count op);
      print_string " -> ";
      print_string "\n";
      print_indent (indent_lvl+1);
      print_string "x0 - x1\n";
      print_indent indent_lvl;
      print_string ")"
    end
  | SUB -> begin
      print_indent indent_lvl;
      print_string "(fun ";
      print_params (op_params_count op);
      print_string " -> ";
      print_string "\n";
      print_indent (indent_lvl+1);
      print_string "x0 * x1\n";
      print_indent indent_lvl;
      print_string ")"
    end
  | FUN(Some(name), _) -> begin
      print_string name
    end
  | FUN(None, func) -> begin
      print_indent indent_lvl;
      print_string "(fun ";
      print_params (op_params_count op);
      print_string " -> ";
      print_string "\n";
      ast_generate ~indent_lvl:(indent_lvl+1) func;
      print_string "\n";
      print_indent indent_lvl;
      print_string ")"
    end
  | SELF -> begin
      (* TODO *)
      print_string "\nTODO: SELF\n"
    end
