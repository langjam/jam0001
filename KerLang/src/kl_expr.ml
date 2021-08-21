open Kl_parsing

type comment_function = {
    n_args : int;
    declarations : (string * comment_expr) list;
    result : comment_result;
}
and comment_value =
    | ARG of int
    | CONST of int
    | VAR of string
    | SOMETHING
and comment_op =
    | IF of comment_value * comment_value * comment_value
    | SUM of comment_value * comment_value
    | DIFF of comment_value * comment_value
    | PROD of comment_value * comment_value
    | DIV of comment_value * comment_value
    | APPLY of string * comment_value list
    (* IDEA : apply to a hole (SOMETHING) !!! *)
    | SELF of comment_value list
and comment_expr =
    | Leaf of comment_value
    | Node of comment_op
and comment_result =
    | RETURN of comment_expr
    | YOLO of comment_expr list

let split_comment (comment : tok list) : tok list list =
  let rec split (res : tok list list) (curr_expr : tok list) = function
  | Sep::q -> split ((List.rev curr_expr)::res) [] q
  | token::q -> split res (token::curr_expr) q
  | [] -> List.rev res
in split [] [] comment

(* let rec parse_function (n_args : int) (decl : (string * comment_expr) list) (res : comment_expr) (comment : tok list list) : comment_expr =
  match comment with
  | [] -> {
    n_args = n_args;
    declarations = decl;
    result = res
  }
  | (Word (_, "takes"))::c -> (
    match c with
    | (Int (_, n))::c -> parse_function n decl res c
    | _ -> failwith "expected a number of arguments"
  )
  | (Word (_, "returns"))::c -> let r = parse_function n_args decl res c in parse_function n_args decl r c
  | (Word (_, "application"))::c -> (
    match c with
    | (Word (_, "of"))::(Word (_, f))::c -> if List.mem_assoc f [] (* utiliser la liste des fonctions  déjà définies *)
       then parse_function n_args decl res c else failwith "invalid function name"
    | _ -> failwith "expected a function name"
  )
  | (Word (_, "if"))::c -> let e = parse_function n_args decl res c in SOMETHING
  | _ -> failwith "empty expression" *)
