(**
  {1 The glorious KerLang Compiler}
  
  also known as the glorious Ker-Lann Compiler but who cares ?
*)

let main =
  let input = Lexing.from_string "/* a function */ function f; /* a yolo function */ yolo g;" in
  let toks = ref [] in
  try while true do
    toks := Kerlang.Kl_parser.block input::!toks
  done;
  with Kerlang.Kl_parser.Eof ->
    List.iter (Format.printf "%a\n" Kerlang.Kl_parser.pp_spec) (List.rev !toks)