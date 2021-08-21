(**
  {1 The glorious KerLang Compiler}
  
  also known as the glorious Ker-Lann Compiler but who cares ?
*)

(*
  Pretty print a position in a lexbuf.
  Code taken from the book "Real World OCaml".
*)
let print_position outx lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  Printf.fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

(**
  Parse a KerLang file and print the result of the parsing
*)
let parse_and_print f =
  let lexbuf = Lexing.from_channel (open_in f) in
  let blocks = ref [] in
  Lexing.set_filename lexbuf f;
  try while true do
    blocks := Kerlang.Kl_parser.block lexbuf::!blocks
  done;
  with
  | Kerlang.Kl_parser.Eof ->
    List.iter (Format.eprintf "@[%a@]\n\n" Kerlang.Kl_parser.pp_spec) (List.rev !blocks)
  | Kerlang.Kl_parser.SyntaxError msg ->
    Printf.eprintf "\x1b[1;31msyntax error\x1b[0m (%a) : %s\n" print_position lexbuf msg

let usage_msg = Sys.argv.(0) ^ " [-verbose] <file1> -o <output>"

let verbose = ref false

let input_files = ref ""

let output_file = ref ""


let anon_fun filename =
  input_files := filename

let speclist =
  [("-verbose", Arg.Set verbose, "Output debug information");
    ("-o", Arg.Set_string output_file, "Set output file name")]

let () =
  Arg.parse speclist anon_fun usage_msg;
  if !input_files = "" then
    Printf.eprintf "%s\n" usage_msg
  else
    parse_and_print !input_files