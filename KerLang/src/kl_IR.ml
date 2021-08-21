type op = ADD | MUL | SUB | FUN of ast | SELF

and ast =
  | App of op * ast list
  | Cst of int
  | Var of int
  | If of ast * ast * ast

let rec eval_rec (self : ast option) (env : int list) (expr : ast) : int =
  match expr with
  | App (op, args) ->
    List.map (eval_rec self env) args
    |> eval_op None op
  | Cst n -> n
  | Var x -> List.nth env x
  | If (c, b1, b2) ->
    if eval_rec self env c = 0
    then eval_rec self env b2
    else eval_rec self env b1

and eval_op (self : ast option) = function
| ADD -> (function [x; y] -> x + y | _ -> failwith "ADD : wrong number of args")
| SUB -> (function [x; y] -> x - y | _ -> failwith "SUB : wrong number of args")
| MUL -> (function [x; y] -> x * y | _ -> failwith "MUL : wrong number of args")
| FUN body -> (fun args -> eval_rec (Some body) args body)
| SELF ->
  match self with
  | Some body -> eval_op self (FUN body)
  | None -> failwith "'self' is unbound"

let eval = eval_rec None
let[@inline] add x y = App (ADD, [x; y])
let[@inline] sub x y = App (SUB, [x; y])
let[@inline] mul x y = App (MUL, [x; y])
let[@inline] app f args = App (FUN f, args)

let rec pp_ast fmt = function
  | App (op, args) ->
    Format.fprintf fmt "(%a %a)" pp_op op pp_list args
  | Cst n ->
    Format.pp_print_int fmt n
  | Var x ->
    Format.fprintf fmt "x%d" x
  | If (c, e1, e2) ->
    Format.fprintf fmt "(if %a %a %a)" pp_ast c pp_ast e1 pp_ast e2

and pp_op fmt = function
  | ADD -> Format.pp_print_string fmt "+"
  | SUB -> Format.pp_print_string fmt "-"
  | MUL -> Format.pp_print_string fmt "×"
  | FUN a -> Format.fprintf fmt "%a" pp_ast a
  | SELF -> Format.pp_print_string fmt "self"

and pp_list fmt = function
    | [] -> ()
    | [x] -> pp_ast fmt x
    | x::xs -> Format.fprintf fmt "%a %a" pp_ast x pp_list xs

let test = app (mul (Cst 2) (Var 0)) [add (Cst 1) (Cst 2)]