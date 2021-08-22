open Core

let evaluate source_filepath phrase_str =
  let result =
    let open Or_error.Let_syntax in
    print_endline "parsing phrase..." ;
    let%bind phrase = Parser.(parse (articulated phrase) phrase_str) in
    print_endline "loading progam..." ;
    let%bind program =
      Program.load_primitives Program.empty_program
        ~primitives:Primitives.v
        ~primitive_verb_mappings:Primitives.verb_mappings
    in
    let%bind program =
      let filepath = Filename.to_absolute_exn ~relative_to:(Sys.getcwd ()) source_filepath in
      Program.load_file program ~filepath
    in
    print_endline "evaluating program..." ;
    let%map (result, _) = Interpreter.call_phrase ~program ~env:Value.empty_env phrase in
    result
  in
  Out_channel.(flush stdout) ;
  match result with
  | Ok value -> Printf.printf "result = %s\n" Value.(value |> normalize |> pretty_to_string)
  | Error err -> Printf.eprintf "!!! FATAL ERROR: %s\n" (Error.to_string_hum err)

let () =
  let open Cmdliner in
  let eval_term =
    let open Arg in
    let source_filepath =
      let doc = "Filepath to source code to load for evaluation." in
      required
      & pos 0 (some file) None
      & info [] ~docv:"FILEPATH" ~doc
    in
    let phrase =
      let doc = "Phrase to evaluate." in
      required
      & pos 1 (some string) None
      & info [] ~docv:"PHRASE" ~doc
    in
    Term.(const evaluate $ source_filepath $ phrase)
  in
  let eval_info = Term.info "coqui" in
  Term.(exit @@ eval (eval_term, eval_info))