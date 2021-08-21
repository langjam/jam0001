{
open Lexing

type tok = Int of int | Word of string
type spec = Spec of bool * string * tok list

let pp_tok fmt = function
  | Int i -> Format.fprintf fmt "(Int %d)" i
  | Word i -> Format.fprintf fmt "(Id %s)" i

let pp_spec fmt (Spec (b, f, xs)) =
  Format.fprintf fmt "Spec [strict = %B] of %s is [%s]" b f @@
  String.concat " " @@
  List.map (Format.asprintf "%a" pp_tok) xs

exception SyntaxError of string
exception Eof

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = pos.pos_cnum;
               pos_lnum = pos.pos_lnum + 1
    }
}

let int = '-'? ['0'-'9'] ['0'-'9']*
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let word = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule block = parse
  | white     { block lexbuf }
  | newline   { next_line lexbuf; block lexbuf }
  | "/*"      { comment (ref []) lexbuf }
  | _         {
    let c = Lexing.lexeme lexbuf in
    raise (SyntaxError (
      "Unexcepected character '"
      ^ c
      ^ "', expected comment block"))
  }
  | eof       { raise Eof }

and comment l = parse
  | white     { comment l lexbuf }
  | newline   { next_line lexbuf; comment l lexbuf }
  | "*/"      { defun (List.rev !l) lexbuf }
  | word      { l := (Word (Lexing.lexeme lexbuf))::!l; comment l lexbuf }
  | int       { l := (Int (int_of_string (Lexing.lexeme lexbuf)))::!l; comment l lexbuf }
  | _         {
    let c = Lexing.lexeme lexbuf in
    raise (SyntaxError ("invalid token '" ^ c ^ "' found while parsing a comment"))
  }
  | eof       { raise (SyntaxError "unexcpected enf of file while parsing a comment") }

and defun l = parse
  | white     { defun l lexbuf }
  | newline   { next_line lexbuf; defun l lexbuf }
  | "function " (word as fname) ";" { Spec (true, fname, l) }
  | "yolo " (word as fname) ";" { Spec (false, fname, l) }
  | _         {
    let c = Lexing.lexeme lexbuf in
    raise (SyntaxError ("invalid token '" ^ c ^ "' found while parsing a comment"))
  }
  | eof       { raise (SyntaxError "unexcpected enf of file while parsing a function declaration") }