open Kl_parsing
open Kl_errors

exception InterruptParsing

type expr =
  | Leaf of value
  | Node of operation

and value =
  | Arg of int
  | Cst of int
  | Var of string
  | Hole

and operation =
  | If of expr * expr * expr
  | Sum of expr * expr
  | Diff of expr * expr
  | Prod of expr * expr
  | Div of expr * expr
  | App of string * expr list
  | Rec of expr list

type cconstraint =
  | Takes of int
  | Let of string * expr
  | Shows of expr list
  | Returns of expr
  | Uses of expr list
  | Nothing

let rec pp_expr oc (expr : expr) =
  match expr with
  | Leaf value -> pp_value oc value
  | Node operation -> pp_operation oc operation

and pp_value oc (value: value) =
  match value with
  | Arg id -> Format.fprintf oc "x%d" (id - 1)
  | Cst value -> Format.fprintf oc "%d" value
  | Var name -> Format.fprintf oc "%s" name
  | Hole -> Format.fprintf oc "{??}"

and pp_operation oc (operation : operation) =
  match operation with
  | If (cond, ifcase, elsecase) ->
    Format.fprintf oc "(if %a then %a else %a)"
      pp_expr cond
      pp_expr ifcase
      pp_expr elsecase
  | Sum (a, b) ->
    Format.fprintf oc "(%a + %a)" pp_expr a pp_expr b
  | Diff (a, b) ->
    Format.fprintf oc "(%a - %a)" pp_expr a pp_expr b
  | Prod (a, b) ->
    Format.fprintf oc "(%a * %a)" pp_expr a pp_expr b
  | Div (a, b) ->
    Format.fprintf oc "(%a / %a)" pp_expr a pp_expr b
  | App (func_name, value_list) ->
    Format.fprintf oc "(%s " func_name;
    List.iter (fun value -> Format.fprintf oc "%a " pp_expr value) value_list;
    Format.fprintf oc ")"
  | Rec value_list ->
    Format.fprintf oc "(rec ";
    List.iter (fun value -> Format.fprintf oc "%a " pp_expr value) value_list;
    Format.fprintf oc ")"

let pp_cconstraint oc (cc: cconstraint) =
  match cc with
  | Takes how_many_args ->
    Format.fprintf oc "- Takes %d arguments\n" how_many_args
  | Let (var_name, expr) ->
    Format.fprintf oc "- Let %s be %a" var_name pp_expr expr
  | Shows expr_list ->
    Format.fprintf oc "- Shows";
    List.iter (fun expr -> Format.fprintf oc "%s" "\n  * "; pp_expr oc expr) expr_list
  | Returns expr ->
    Format.fprintf oc "- Returns %a" pp_expr expr
  | Uses expr_list ->
    Format.fprintf oc "- Uses";
    List.iter (fun expr -> Format.fprintf oc "%s" "\n  * "; pp_expr oc expr) expr_list
  | Nothing ->
    Format.fprintf oc "- Has comments for humans\n"

type function_result =
  | Function of expr
  | Yolo of expr list

and comment_function = {
  name : string;
  n_args : int;
  declarations : (string * expr) list;
  printing : expr list;
  result : function_result;
}

let is_hole (v : value) =
  match v with | Arg _ | Cst _ | Var _ -> false | Hole -> true

let rec has_holes = function
  | Leaf Hole -> true
  | Leaf _ -> false
  | Node op ->
    match op with
    | If (a, b, c) -> has_holes a || has_holes b || has_holes c
    | Sum (a, b) -> has_holes a || has_holes b
    | Diff (a, b) -> has_holes a || has_holes b
    | Prod (a, b) -> has_holes a || has_holes b
    | Div (a, b) -> has_holes a || has_holes b
    | App (_, l) | Rec l -> List.exists has_holes l

let print_comment_function {name; n_args; declarations; result; _} =
  Printf.printf "- info :\n  args: %d\n  declarations: [%s]\n"
    n_args (List.map (fun (n, e) -> Format.asprintf "%s -> %a" n pp_expr e) declarations |> String.concat ", ");
  match result with
  | Yolo ctx ->
    Printf.printf "- constraints for %s are'nt sufficient to build a function\n" name;
    Printf.printf "- known context to synthesize a function:\n";
    List.iteri (fun i expr ->
        Format.printf "  %d : %a\n" i pp_expr expr) ctx
  | Function e ->
    Printf.printf "- known context:\n";
    List.iter (fun (name, expr) ->
        Format.printf "  \"%s\" : %a\n" name pp_expr expr) declarations;
    Printf.printf "- generated function is:\n";
    Format.printf "  %a\n" pp_expr e;
    if has_holes e then begin
      Printf.printf "- there are remaining holes in the function !\n";
      Printf.printf "  let's try to complete holes from context !\n"
    end

let split_lines (comment : tok list) : tok list list =
  let rec split (res : tok list list) (curr_expr : tok list) = function
    | Sep::q -> split ((List.rev curr_expr)::res) [] q
    | token::q -> split res (token::curr_expr) q
    | [] -> List.rev res
  in split [] [] comment

let tok_pos = function
  | Int (p, _) | Word (p, _) -> p
  | Sep -> dev_error "sep has no position"

let string_of_tok = function
  | Int (_, n) -> string_of_int n
  | Word (_, w) -> String.lowercase_ascii w
  | Sep -> ""

let rec string_of_comment = function
  | [] -> ""
  | [t] -> string_of_tok t
  | t::q -> (string_of_tok t) ^ " " ^ (string_of_comment q)

exception KeywordNotFound of string

let look_for (kw : string) (comment : tok list) : tok list * tok list =
  let rec search (prev : tok list) = function
    | [] -> raise (KeywordNotFound ("expected keyword " ^ kw))
    | t::q -> if string_of_tok t = kw then (List.rev prev, q)
      else search (t::prev) q
  in search [] comment

let split_kw (kw : string) (comment : tok list) : tok list list =
  let rec split (res : tok list list) (curr_expr : tok list) = function
    | [] -> List.rev ((List.rev curr_expr)::res)
    | t::q -> if string_of_tok t = kw then split ((List.rev curr_expr)::res) [] q
      else split res (t::curr_expr) q
  in split [] [] comment

let parse_value (decl : (string * expr) list) (comment : tok list) : value =
  let rec build_value res = function
    | [] -> if List.mem_assoc res decl then Var res
      else raise (SyntaxError (tok_pos (List.hd comment), "no variable named " ^ res))
    | t::q ->
      begin match string_of_tok t with
        | "something" -> Hole
        | s -> build_value (res ^ " " ^ s) q
      end
  in match comment with
  | [] -> Hole
  | (Int (_, n))::_ -> Cst n
  | t::q ->
    match string_of_tok t with
    | "argument" ->
      begin match q with
        | Int (_, n)::_ -> Arg n
        | _ -> raise (SyntaxError (tok_pos t, "expected argument index"))
      end
    | "something" -> Hole
    | s -> build_value s q

let parse_operation ftable (decl : (string * expr) list) (comment : tok list) : operation =
  let f x = Leaf (parse_value decl x) in
  let rec search (prev : tok list) = function
    | [] -> raise InterruptParsing
    | t::q -> (
        match string_of_tok t with
        | "if" ->
          begin try
              let a, b = look_for "and" q in
              let b, _ = look_for "otherwise" b in
              If (f a, f (List.rev prev), f b)
            with KeywordNotFound msg -> raise (SyntaxError (tok_pos t, msg)) end
        | "sum" ->
          begin try
              let _, b = look_for "of" q in
              let a, b = look_for "and" b in
              Sum (f a, f b)
            with KeywordNotFound msg -> raise (SyntaxError (tok_pos t, msg)) end
        | "difference" ->
          begin try
              let _, b = look_for "of" q in
              let a, b = look_for "and" b in
              Diff (f a, f b)
            with KeywordNotFound msg -> raise (SyntaxError (tok_pos t, msg)) end
        | "product" ->
          begin try
              let _, b = look_for "of" q in
              let a, b = look_for "and" b in
              Prod (f a, f b)
            with KeywordNotFound msg -> raise (SyntaxError (tok_pos t, msg)) end
        | "division" ->
          begin try
              let _, b = look_for "of" q in
              let a, b = look_for "and" b in
              Div (f a, f b)
            with KeywordNotFound msg -> raise (SyntaxError (tok_pos t, msg)) end
        | "application" ->
          begin try
              let _, b = look_for "of" q in
              let a, b = look_for "on" b in
              let s = string_of_comment a and l = split_kw "and" b in
              if s = "self" then Rec (List.map f l)
              else if List.mem_assoc s ftable then App (s, List.map f l)
              else raise (SyntaxError (tok_pos t, "no function named " ^ s))
            with KeywordNotFound msg -> raise (SyntaxError (tok_pos t, msg)) end
        | _ -> search (t::prev) q
      )
  in search [] comment

let rec parse_statement ftable (decl : (string * expr) list) (comment : tok list) : cconstraint =
  let f x = try Node (parse_operation ftable decl x) with InterruptParsing -> Leaf (parse_value decl x) in
  match comment with
  | [] -> Nothing
  | t::q ->
    match string_of_tok t with
    | "take" | "takes" ->
      begin match q with
        | Int (_, n)::_ -> Takes n
        | _ -> raise (SyntaxError (tok_pos t, "expected number of arguments"))
      end
    | "let" ->
      begin try
          let a, b = look_for "be" q in Let (string_of_comment a, f b)
        with KeywordNotFound msg -> raise (SyntaxError (tok_pos t, msg)) end
    | "show" | "shows" -> let l = split_kw "and" q in Shows (List.map f l)
    | "return" | "returns" -> Returns (f q)
    | "use" | "uses" -> let l = split_kw "and" q in Uses (List.map f l)
    | _ -> parse_statement ftable decl q

let generate_function ftable (Spec (_, name, comment)) =
  Printf.printf "- generating function %s\n" name;
  let comments = split_lines comment in
  let rec build_function (f : comment_function) = function
    | [] -> f
    | c::q ->
      match parse_statement ftable f.declarations c with
      | Nothing -> build_function f q
      | Takes n -> build_function {f with n_args = n} q
      | Let (s, e) -> build_function {f with declarations = (s, e)::f.declarations} q
      | Shows l -> build_function {f with printing = f.printing @ l} q
      | Returns e ->
        let r = match f.result with
          | Yolo [] -> Function e
          | Yolo _ ->
            begin
              located_warning (tok_pos (List.hd c)) (name ^ " was in yolo mode, now using return value");
              Function e
            end
          | Function _ ->
            begin
              located_warning (tok_pos (List.hd c)) (name ^ " already has a return value, still using old return value");
              f.result
            end
        in build_function {f with result = r} q
      | Uses l ->
        let r = match f.result with
          | Yolo [] -> Yolo l
          | Yolo l' -> Yolo (l' @ l)
          | Function e ->
            begin
              located_warning (tok_pos (List.hd c)) (name ^ " already has a return value, ignoring yolo mode");
              Function e
            end
        in build_function {f with result = r} q
  in
  let res = build_function {
      name; n_args = 0; declarations = []; printing = []; result = Yolo []
    } comments
  in
  print_comment_function res; res


let rec complete_holes decl (remain : expr list) = function
  | Leaf Hole ->
    begin match remain with
      | [] -> raise InterruptParsing
      | e::q -> complete_holes decl q e
    end
  | Leaf (Var s) -> complete_holes decl remain (List.assoc s decl)
  | Leaf v -> Leaf v, remain
  | Node op ->
    match op with
    | If (a, b, c) ->
      let a, remain = complete_holes decl remain a in
      let b, remain = complete_holes decl remain b in
      let c, remain = complete_holes decl remain c in
      (Node (If (a, b, c))), remain
    | Sum (a, b) ->
      let a, remain = complete_holes decl remain a in
      let b, remain = complete_holes decl remain b in
      (Node (Sum (a, b))), remain
    | Diff (a, b) ->
      let a, remain = complete_holes decl remain a in
      let b, remain = complete_holes decl remain b in
      (Node (Diff (a, b))), remain
    | Prod (a, b) ->
      let a, remain = complete_holes decl remain a in
      let b, remain = complete_holes decl remain b in
      (Node (Prod (a, b))), remain
    | Div (a, b) ->
      let a, remain = complete_holes decl remain a in
      let b, remain = complete_holes decl remain b in
      (Node (Div (a, b))), remain
    | App (s, l) ->
      let r = ref remain in
      let f e = let u, v = complete_holes decl !r e in r := v; u in
      let l = List.map f l in
      (Node (App (s, l))), !r
    | Rec l ->
      let r = ref remain in
      let f e = let u, v = complete_holes decl !r e in r := v; u in
      let l = List.map f l in
      (Node (Rec l)), !r

let rec compile_yolo decl = function
  | [] -> raise (CompileError "unable to synthesize the function !")
  | e::q ->
    try let res, _ = complete_holes decl q e in res
    with InterruptParsing -> compile_yolo decl q

let rec compile_function (ftable : Kl_IR.ftable) { result; declarations; printing; _ } =
  let compile_ex = compile_expr ftable declarations in
  let res = match result with
  | Function e ->
    Printf.printf "- compiling the function to KL_IR\n";
    compile_ex e
  | Yolo l ->
    Printf.printf "- performing function synthesis\n";
    compile_ex (compile_yolo declarations l)
  in let rec add_printing res = function
  | [] -> res
  | e::q -> add_printing (Kl_IR.show (compile_ex e) res) q
  in add_printing res printing

and compile_expr ftable env = function
  | Leaf v -> compile_value ftable env v
  | Node v -> compile_operation ftable env v

and compile_value ftable env = function
  | Arg x -> Kl_IR.Var (x - 1)
  | Cst n -> Kl_IR.Cst n
  | Var x -> Kl_IR.elookup x env |> compile_expr ftable env
  | Hole -> raise (CompileError "remaining hole in expression, can't compile it down to Kl_IR")

and compile_operation ftable env =
  let compile_ex = compile_expr ftable env in
  function
  | If (cond, br1, br2) ->
    Kl_IR.If (compile_ex cond, compile_ex br1, compile_ex br2)
  | Sum (e1, e2) ->
    Kl_IR.add (compile_ex e1) (compile_ex e2)
  | Diff (e1, e2) ->
    Kl_IR.sub (compile_ex e1) (compile_ex e2)
  | Prod (e1, e2) ->
    Kl_IR.mul (compile_ex e1) (compile_ex e2)
  | Div (e1, e2) ->
    Kl_IR.div (compile_ex e1) (compile_ex e2)
  | App (fname, vals) ->
    Kl_IR.app (FUN fname) (List.map compile_ex vals)
  | Rec vs -> Kl_IR.app Kl_IR.SELF (List.map compile_ex vs)
