{
open Lexing
open Kl_parsing

exception SyntaxError of string
exception Eof
}

let int = '-'? ['0'-'9'] ['0'-'9']*
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let sep = ',' | '.' | '!' | '?'
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
  | word      { l := (Word (lexbuf.lex_curr_p, Lexing.lexeme lexbuf))::!l; comment l lexbuf }
  | int       { l := (Int (lexbuf.lex_curr_p, int_of_string (Lexing.lexeme lexbuf)))::!l; comment l lexbuf }
  | sep       { l := Sep::!l; comment l lexbuf }
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
    raise (SyntaxError ("invalid token '" ^ c ^ "' found while parsing a function definition"))
  }
  | eof       { raise (SyntaxError "unexcpected enf of file while parsing a function declaration") }