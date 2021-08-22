open Kl_parsing

type value =
  | Arg of int
  | Cst of int
  | Var of string
  | Hole

type operation =
  | If of value * value * value
  | Sum of value * value
  | Diff of value * value
  | Prod of value * value
  | Div of value * value
  | App of string * value list
  (* IDEA : apply to a hole (SOMETHING) !!! *)
  | Rec of value list

type expr =
  | Leaf of value
  | Node of operation

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

let parse_value (comment : tok list) : value =
  match comment with
  | [] -> Hole
  | (Int (_, n))::_ -> Cst n
  | t::q ->
    let kwd = string_of_tok t in
    if kwd = "argument" then begin
      match q with
      | Int (_, n)::_ -> Arg n
      | _ -> print_syntax_error (tok_pos t) "expected argument index"
    end else if kwd = "something" then
      Hole
    else Var kwd
    (* TODO check variable name *)

let parse_operation (comment : tok list) : operation =
  let f = parse_value in
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

let rec parse_statement (comment : tok list) : cconstraint =
  let f x = try Node (parse_operation x) with _ -> Leaf (parse_value x) in
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
    | _ -> parse_statement q

let generate_function (Spec (_, name, comment)) =
  let comments = split_lines comment in
  let rec build_function (f : comment_function) = function
    | [] -> f
    | c::q ->
      match parse_statement c with
      | Nothing -> build_function f q
      | Takes n -> build_function {f with n_args = n} q
      | Let (s, e) -> build_function {f with declarations = (s, e)::f.declarations} q
      | Returns e -> build_function {f with result = Function e} q
      | Uses l ->
        let r = match f.result with | Function e -> Yolo (e::l) | Yolo l' -> Yolo (l @ l')
        in build_function {f with result = r} q
  in build_function {
    name; n_args = 0; declarations = []; result = Yolo []
  } comments

let rec compile_function ftable { result; declarations; _ } =
  match result with
  | Function e -> compile_expr ftable declarations e
  | _ -> Kl_IR.Cst 0

and compile_expr ftable env = function
  | Leaf v -> compile_value ftable env v
  | Node v -> compile_operation ftable env v

and compile_operation ftable env =
  let compile_val = compile_value ftable env in
  function
  | If (cond, br1, br2) ->
    Kl_IR.If (compile_val cond, compile_val br1, compile_val br2)
  | Sum (e1, e2) ->
    Kl_IR.add (compile_val e1) (compile_val e2)
  | Diff (e1, e2) ->
    Kl_IR.sub (compile_val e1) (compile_val e2)
  | Prod (e1, e2) ->
    Kl_IR.mul (compile_val e1) (compile_val e2)
  | Div (e1, e2) ->
    Kl_IR.div (compile_val e1) (compile_val e2)
  | App (fname, vals) ->
    Kl_IR.(app (func ~name:(Some fname) (List.assoc fname ftable)) (List.map compile_val vals))
  | Rec vs -> Kl_IR.app Kl_IR.SELF (List.map compile_val vs)

and compile_value ftable env = function
  | Arg x -> Kl_IR.Var (x - 1)
  | Cst n -> Kl_IR.Cst n
  | Var x -> List.assoc x env |> compile_expr ftable env
  | Hole -> failwith "remaning hole in expression, can't compile it down to Kl_IR"
