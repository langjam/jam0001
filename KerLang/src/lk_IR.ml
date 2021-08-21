type op = ADD | MUL | SUB | FUN of ast

and ast =
  | Arith of op * ast list
  | Cst of int
  | Var of int
  | If of ast * ast * ast


let rec eval (env : int list) (a : ast) : int =
  match a with
  | Arith (op, args) ->
    List.map (eval env) args
    |> eval_op op
  | Cst x -> x
  | Var x -> List.nth env x
  | If (c, b1, b2) ->
    if eval env c = 0 then eval env b2 else eval env b1

and eval_op = function
| ADD -> (function [x; y] -> x + y | _ -> failwith "ADD : wrong number of args")
| SUB -> (function [x; y] -> x - y | _ -> failwith "SUB : wrong number of args")
| MUL -> (function [x; y] -> x * y | _ -> failwith "MUL : wrong number of args")
| FUN a -> (fun args -> eval args a)

let f = FUN (Arith (MUL, [Cst 2; Var 0]))