open Kl_parsing

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
  | Returns of expr
  | Uses of expr list
  | Nothing

type function_result =
  | Function of expr
  | Yolo of expr list

type comment_function = {
  name : string;
  n_args : int;
  declarations : (string * expr) list;
  result : function_result;
}

let split_lines (comment : tok list) : tok list list =
  let rec split (res : tok list list) (curr_expr : tok list) = function
    | Sep::q -> split ((List.rev curr_expr)::res) [] q
    | token::q -> split res (token::curr_expr) q
    | [] -> List.rev res
  in split [] [] comment

let tok_pos = function
  | Int (p, _) | Word (p, _) -> p
  | Sep -> failwith "sep has no position"

let string_of_tok = function
  | Int (_, n) -> string_of_int n
  | Word (_, w) -> String.lowercase_ascii w
  | Sep -> ""

let rec string_of_comment = function
  | [] -> ""
  | [t] -> string_of_tok t
  | t::q -> (string_of_tok t) ^ " " ^ (string_of_comment q)

let look_for (kw : string) (comment : tok list) : tok list * tok list =
  let rec search (prev : tok list) = function
    | [] -> failwith ("expected keyword " ^ kw)
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
  else print_syntax_error (tok_pos (List.hd comment)) ("no variable named " ^ res)
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
      | _ -> print_syntax_error (tok_pos t) "expected argument index"
      end
    | "something" -> Hole
    | s -> build_value s q

let parse_operation (decl : (string * expr) list) (comment : tok list) : operation =
  let f x = Leaf (parse_value decl x) in
  let rec search (prev : tok list) = function
    | [] -> failwith "no operation"
    | t::q -> (
        match string_of_tok t with
        | "if" ->
          begin try
            let a, b = look_for "and" q in
            let b, _ = look_for "otherwise" b in
            If (f a, f (List.rev prev), f b)
          with Failure msg -> print_syntax_error (tok_pos t) msg end
        | "sum" ->
          begin try
            let _, b = look_for "of" q in
            let a, b = look_for "and" b in
            Sum (f a, f b)
          with Failure msg -> print_syntax_error (tok_pos t) msg end
        | "difference" ->
          begin try
            let _, b = look_for "of" q in
            let a, b = look_for "and" b in
            Diff (f a, f b)
          with Failure msg -> print_syntax_error (tok_pos t) msg end
        | "product" ->
          begin try
            let _, b = look_for "of" q in
            let a, b = look_for "and" b in
            Prod (f a, f b)
          with Failure msg -> print_syntax_error (tok_pos t) msg end
        | "division" ->
          begin try
            let _, b = look_for "of" q in
            let a, b = look_for "and" b in
            Div (f a, f b)
          with Failure msg -> print_syntax_error (tok_pos t) msg end
        | "application" ->
          begin try
            let _, b = look_for "of" q in
            let a, b = look_for "on" b in
            let s = string_of_comment a and l = split_kw "and" b in
            if s = "self" then Rec (List.map f l) else App (s, List.map f l)
          with Failure msg -> print_syntax_error (tok_pos t) msg end
        | _ -> search (t::prev) q
      )
  in search [] comment

let rec parse_statement (decl : (string * expr) list) (comment : tok list) : cconstraint =
  let f x = try Node (parse_operation decl x) with _ -> Leaf (parse_value decl x) in
  match comment with
  | [] -> Nothing
  | t::q ->
    match string_of_tok t with
    | "take" | "takes" ->
      begin match q with
        | Int (_, n)::_ -> Takes n
        | _ -> print_syntax_error (tok_pos t) "expected number of arguments"
      end
    | "let" ->
      begin try
        let a, b = look_for "be" q in Let (string_of_comment a, f b)
      with Failure msg -> print_syntax_error (tok_pos t) msg end
    | "return" | "returns" -> Returns (f q)
    | "use" | "uses" -> let l = split_kw "and" q in Uses (List.map f l)
    | _ -> parse_statement decl q

let generate_function (Spec (_, name, comment)) =
  let comments = split_lines comment in
  let rec build_function (f : comment_function) = function
    | [] -> f
    | c::q ->
      match parse_statement f.declarations c with
      | Nothing -> build_function f q
      | Takes n -> build_function {f with n_args = n} q
      | Let (s, e) -> build_function {f with declarations = (s, e)::f.declarations} q
      | Returns e ->
        let r = match f.result with
        | Yolo [] -> Function e
        | Yolo _ ->
          begin
            print_warning (tok_pos (List.hd c)) (name ^ " was in yolo mode, now using return value");
            Function e
          end
        | Function _ ->
          begin
            print_warning (tok_pos (List.hd c)) (name ^ " already has a return value, still using old return value");
            f.result
          end
        in build_function {f with result = r} q
      | Uses l ->
        let r = match f.result with
        | Yolo [] -> Yolo l
        | Yolo l' -> Yolo (l' @ l)
        | Function e ->
          begin
            print_warning (tok_pos (List.hd c)) (name ^ " already has a return value, ignoring yolo mode");
            Function e
          end
        in build_function {f with result = r} q
  in build_function {
    name; n_args = 0; declarations = []; result = Yolo []
  } comments

let rec complete_holes decl (remain : expr list) = function
  | Leaf Hole ->
    begin match remain with
    | [] -> failwith "unable to complete holes"
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
  | [] -> print_compile_error "unable to complete yolo"
  | e::q ->
    try let res, _ = complete_holes decl q e in res
    with Failure _ -> compile_yolo decl q

let rec compile_function ftable { result; declarations; _ } =
  match result with
  | Function e -> compile_expr ftable declarations e
  | Yolo l -> compile_expr ftable declarations (compile_yolo declarations l)

and compile_expr ftable env = function
  | Leaf v -> compile_value ftable env v
  | Node v -> compile_operation ftable env v

and compile_value ftable env = function
  | Arg x -> Kl_IR.Var (x - 1)
  | Cst n -> Kl_IR.Cst n
  | Var x -> List.assoc x env |> compile_expr ftable env
  | Hole -> print_compile_error "remaning hole in expression, can't compile it down to Kl_IR"

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
    Kl_IR.(app (func ~name:(Some fname) (List.assoc fname ftable)) (List.map compile_ex vals))
  | Rec vs -> Kl_IR.app Kl_IR.SELF (List.map compile_ex vs)
