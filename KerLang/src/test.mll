{
open Lexing

type tok = INT of int | ID of string | EOF

exception SyntaxError of string

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
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule token = parse
  | white     { token lexbuf }
  | newline   { next_line lexbuf; token lexbuf }
  | int       { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | "/*"      { comment lexbuf }
  | eof       { EOF }
  | _         { raise SyntaxError "Invalid character: " ^ Lexing.lexeme lexbuf }

and comment = parse
  | "*/"      { token lexbuf }
  | id        { ID (Lexing.lexeme lexbuf) }
  | white     { comment lexbuf }
  | newline   { next_line lexbuf; comment lexbuf }
  | _         { raise SyntaxError "comment is'nt terminated" }