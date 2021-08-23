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
let print_position outx pos =
  Printf.fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let print_syntax_error pos msg =
  Printf.eprintf "\x1b[1;31msyntax error\x1b[0m (%a) : %s\n" print_position pos msg;
  exit 1

let print_compile_error msg =
  Printf.eprintf "\x1b[1;31mcompile error\x1b[0m : %s\n" msg;
  exit 1

let print_warning pos msg =
  Printf.eprintf "\x1b[1;33mwarning\x1b[0m (%a) : %s\n" print_position pos msg
