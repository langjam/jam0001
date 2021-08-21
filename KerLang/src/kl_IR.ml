(** {1 Kl_IR}
    An expression language designed to be generated by the KerLang compiler
*)

(** {2 Syntax} *)

type op = OUT | ADD | MUL | SUB | FUN of string option * ast | SELF

and ast =
  | App of op * ast list
  | Cst of int
  | Var of int
  | If of ast * ast * ast

type ftable = string * ast list

let[@inline] add x y = App (ADD, [x; y])
let[@inline] sub x y = App (SUB, [x; y])
let[@inline] mul x y = App (MUL, [x; y])
let[@inline] app f args = App (f, args)
let[@inline] func ?name:(name=None) bdy = FUN (name, bdy)
let[@inline] nfunc name ftable = FUN (Some name, List.assoc name ftable)


(** {3 Semantics} *)

let rec eval ?self:(self : ast option = None) (env : int list) (expr : ast) : int =
  match expr with
  | App (op, args) ->
    List.map (eval ~self env) args
    |> eval_op self op
  | Cst n -> n
  | Var x -> List.nth env x
  | If (c, b1, b2) ->
    if eval ~self env c = 0
    then eval ~self env b2
    else eval ~self env b1

and eval_op (self : ast option) = function
  | OUT -> (function [x; next] -> print_int x |> print_newline; next | _ -> failwith "OUT : wrong number of args")
  | ADD -> (function [x; y] -> x + y | _ -> failwith "ADD : wrong number of args")
  | SUB -> (function [x; y] -> x - y | _ -> failwith "SUB : wrong number of args")
  | MUL -> (function [x; y] -> x * y | _ -> failwith "MUL : wrong number of args")
  | FUN (_, body) -> (fun args -> eval ~self:(Some body) args body)
  | SELF ->
    match self with
    | Some body -> eval_op self (func body)
    | None -> failwith "'self' is unbound"

and pp_ast fmt = function
  | App (op, args) ->
    Format.fprintf fmt "@[(%a@, @[%a@])@]" pp_op op pp_list args
  | Cst n ->
    Format.pp_print_int fmt n
  | Var x ->
    Format.fprintf fmt "x%d" x
  | If (c, e1, e2) ->
    Format.fprintf fmt "(if @[%a@]@, @[%a@]@, @[%a@])" pp_ast c pp_ast e1 pp_ast e2

and pp_op fmt = function
  | OUT -> Format.pp_print_string fmt "out"
  | ADD -> Format.pp_print_string fmt "+"
  | SUB -> Format.pp_print_string fmt "-"
  | MUL -> Format.pp_print_string fmt "×"
  | FUN (fn, a) ->
    begin match fn with
      | None -> Format.fprintf fmt "%a" pp_ast a
      | Some name -> Format.pp_print_string fmt name
    end
  | SELF -> Format.pp_print_string fmt "self"

and pp_list fmt = function
  | [] -> ()
  | [x] -> pp_ast fmt x
  | x::xs -> Format.fprintf fmt "%a @,%a" pp_ast x pp_list xs

let test = app (func @@ mul (Cst 2) (Var 0)) [add (Cst 1) (Cst 2)]

let count = func ~name:(Some("count")) @@
  If (Var 0,
      app OUT [Var 0; app SELF [sub (Var 0) (Cst 1)]],
      app OUT [Cst 0; Cst 0])