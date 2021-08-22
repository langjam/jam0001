(**
   {1 The glorious KerLang Compiler}

   also known as the glorious Ker-Lann Compiler but who cares ?
*)

(** Parse a KerLang file and print the result of the parsing *)
let parse_file f =
  let lexbuf = Lexing.from_channel (open_in f) in
  let blocks = ref [] in
  Lexing.set_filename lexbuf f;
  let open Kerlang.Kl_parsing in
  try while true do
      blocks := Kerlang.Kl_parser.block lexbuf::!blocks
    done; []
  with
  | Kerlang.Kl_parser.Eof -> List.rev !blocks
  | Kerlang.Kl_parser.SyntaxError msg ->
    let pos = lexbuf.Lexing.lex_curr_p in
    print_syntax_error pos msg

let usage_msg = Sys.argv.(0) ^ " [-verbose] <srcfile> -o <output>"

let verbose = ref false
let input_files = ref ""
let output_file = ref ""
let output_lang : Kerlang.Kl_codegen.lang option ref = ref None
let error msg = raise (Arg.Bad msg)

let check () =
  if !output_file = "" then
    error "no output file provided"

let anon_fun filename =
  input_files := filename

let set_out_lang s =
  begin match Filename.extension s with
    | ".ml" -> output_lang := Some ML
    | ".py" -> output_lang := Some PY
    | ".c" -> output_lang := Some C
    | _ as ext ->
      error ("Unknwon file extension '" ^ ext ^ "' (known extensions are .ml .py .c")
  end;
  output_file := s


let speclist =
  [("-verbose", Arg.Set verbose, "Output debug information");
   ("-o", Arg.String set_out_lang, "Set output file name, extension must be .ml, .py or .c")]

let () =
  Arg.parse speclist anon_fun usage_msg;
  begin try check () with Arg.Bad msg ->
    Printf.eprintf "\x1b[1;31merror:\x1b[0m %s\n\n" msg;
    Arg.usage speclist usage_msg;
    exit 1
  end;
  parse_file !input_files
  |> Kerlang.Kl_codegen.emit_kl_ir
  |> Kerlang.Kl_codegen.realize (open_out !output_file) (Option.get !output_lang)
