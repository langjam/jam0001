open Lexing

(**
  Parsing utils
*)

type tok = Int of Lexing.position * int | Word of Lexing.position * string | Sep
type spec = Spec of bool * string * tok list

let pp_tok fmt = function
  | Int (_, i) -> Format.fprintf fmt "%d" i
  | Word (_, i) -> Format.fprintf fmt "%s" i
  | Sep -> ()

let pp_spec fmt (Spec (b, f, xs)) =
  Format.fprintf fmt "Spec [strict = %B] of %s is [%s]" b f @@
  String.concat " " @@
  List.map (Format.asprintf "%a" pp_tok) xs

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = pos.pos_cnum;
               pos_lnum = pos.pos_lnum + 1
    }

(*
  Pretty print a position in a lexbuf.
  Code taken from the book "Real World OCaml".
*)
let print_position outx lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  Printf.fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)