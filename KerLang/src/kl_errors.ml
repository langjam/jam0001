(**
   Centralized exception handling for the KerLang compiler
*)

open Kl_parsing

exception SyntaxError of (Lexing.position * string)
exception CompileError of string
exception DeveloperError of string

let warning msg =
  Printf.eprintf "\x1b[1;33mwarning:\x1b[0m %s\n" msg

let error msg =
  Printf.eprintf "\x1b[1;31merror:\x1b[0m %s\n" msg;
  exit 1

let dev_error msg =
  Printf.eprintf "\x1b[1;31mimplementation-bug-error:\x1b[0m %s\n" msg;
  Printf.eprintf "\x1b[1;36mnote:\x1b[0m this is a bug in the KerLang implementation, this is not your fault\n";
  exit 1

let syntax_error pos msg =
  Printf.eprintf "\x1b[1;31merror: (%a) \x1b[0m %s\n" print_position pos msg;
  exit 1

let located_warning pos msg =
  Printf.eprintf "\x1b[1;33mwarning: (%a) \x1b[0m %s\n" print_position pos msg

