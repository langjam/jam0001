{
open Lexing

type tok = INT of int | ID of string | LIST of tok list

let pp_tok fmt = function
  | INT i -> Format.fprintf fmt "Int (%d)" i
  | ID i -> Format.fprintf fmt "Id (%s)" i
  | LIST _ -> Format.fprintf fmt "LS []"

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

rule token = parse
  | white     { token lexbuf }
  | newline   { next_line lexbuf; token lexbuf }
  | int       { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | '('       { LIST (comment (ref []) lexbuf) }
  | eof       { raise Eof }
  | _         { raise (SyntaxError ("Invalid character: " ^ (Lexing.lexeme lexbuf))) }

and comment l = parse
  | ')'      { List.rev !l }
  | word     { l := (ID (Lexing.lexeme lexbuf))::!l; comment l lexbuf }
  | white     { comment l lexbuf }
  | newline   { next_line lexbuf; comment l lexbuf }
  | _         { raise (SyntaxError "comment is'nt terminated") }