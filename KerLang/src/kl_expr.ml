open Kl_parsing

type 'a statement =
  | TAKES of 'a
  | LET of string * 'a
  | RETURNS of 'a
  | USES of 'a
  | NONE

type 'a operation =
  | IF of 'a * 'a * 'a
  | SUM of 'a * 'a
  | DIFF of 'a * 'a
  | PROD of 'a * 'a
  | DIV of 'a * 'a
  | APPLY of string * 'a list
  (* IDEA : apply to a hole (SOMETHING) !!! *)
  | SELF of 'a list

type value =
  | ARG of int
  | CONST of int
  | VAR of string
  | SOMETHING

type comment_function = {
  n_args : int;
  declarations : (string * comment_expr) list;
  result : comment_result;
}
and comment_expr =
  | Leaf of value
  | Node of value operation
and comment_result =
  | RETURN of comment_expr
  | YOLO of comment_expr list


let split_lines (comment : tok list) : tok list list =
  let rec split (res : tok list list) (curr_expr : tok list) = function
  | Sep::q -> split ((List.rev curr_expr)::res) [] q
  | token::q -> split res (token::curr_expr) q
  | [] -> List.rev res
in split [] [] comment

let string_of_tok = function
  | Int(_, n) -> string_of_int n
  | Word(_, w) -> w
  | Sep -> ""

let rec string_of_comment = function
  | [] -> ""
  | [t] -> string_of_tok t
  | t::q -> (string_of_tok t) ^ (string_of_comment q)

let look_for (kw : string) (comment : tok list) : tok list * tok list =
  let rec search (prev : tok list) = function
  | [] -> failwith ("keyword" ^ kw ^ "not found")
  | t::q -> if string_of_tok t = kw then (List.rev prev, q)
  else search (t::prev) q
in search [] comment

let split_kw (kw : string) (comment : tok list) : tok list list =
  let rec split (res : tok list list) (curr_expr : tok list) = function
  | [] -> List.rev ((List.rev curr_expr)::res)
  | t::q -> if string_of_tok t = kw then split ((List.rev curr_expr)::res) [] q
  else split res (t::curr_expr) q
in split [] [] comment

let rec parse_statement (comment : tok list) : (tok list) statement =
  match comment with
  | [] -> NONE
  | t::q -> (
    match string_of_tok t with
    | "takes" -> TAKES q
    | "let" -> let a, b = look_for "be" q in LET (string_of_comment a, b)
    | "returns" -> RETURNS q
    | "uses" -> USES q
    | _ -> parse_statement q
  )

let parse_operation (comment : tok list) : (tok list) operation =
  let rec search (prev : tok list) = function
  | [] -> failwith "no operation"
  | t::q -> (
    match string_of_tok t with
    | "if" -> let a, b = look_for "and" q in let b, _ = look_for "otherwise" b in IF (List.rev prev, a, b)
    | "addition" -> let _, b = look_for "of" q in let a, b = look_for "and" b in SUM (a, b)
    | "difference" -> let _, b = look_for "of" q in let a, b = look_for "and" b in DIFF (a, b)
    | "product" -> let _, b = look_for "of" q in let a, b = look_for "and" b in PROD (a, b)
    | "division" -> let _, b = look_for "of" q in let a, b = look_for "and" b in DIV (a, b)
    | "application" -> let _, b = look_for "of" q in let a, b = look_for "on" b in
      let s = string_of_comment a and l = split_kw "and" b in
      if s = "self" then SELF(l) else APPLY (s, l)
    | _ -> search (t::prev) q
  )
in search [] comment

let parse_value (comment : tok list) : value =
  match comment with
  | [] -> SOMETHING
  | t::q -> (
    match t with
    | Int(_, n) -> CONST n
    | _ -> match string_of_tok t with
      | "argument" -> (
        match q with
        | Int(_, n)::_ -> ARG n
        | _ -> failwith "expected argument number"
      )
      | "something" -> SOMETHING
      | s -> VAR s
  )
