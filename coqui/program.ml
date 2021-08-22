open Core
open Value
open Or_error.Let_syntax

type t = Value.program

let empty_program =
  { file_states = String.Map.empty
  ; abstractions = AbstractionSignature.Map.empty
  ; mapped_verbs = String.Set.empty
  ; verb_mappings = String.Map.empty }

let register_abstraction t header block =
  let signature = AbstractionSignature.of_abstraction_header header in
  match Map.add t.abstractions ~key:signature ~data:(header, block) with
  | `Ok abstractions -> Or_error.return {t with abstractions}
  | `Duplicate -> Or_error.errorf "phrase definition collision: %s" (Syntax.show_abstraction_header header)

let register_verb_mapping t {Syntax.simple; progressive} =
  if String.Set.mem t.mapped_verbs simple then
    Or_error.errorf "verb mapping already defined for \"%s\"" simple
  else
    Or_error.return
      { t with
        mapped_verbs = String.Set.add t.mapped_verbs simple
      ; verb_mappings = String.Map.add_exn t.verb_mappings ~key:progressive ~data:simple }

let load_primitives p ~primitives ~primitive_verb_mappings =
  let%bind p =
    Util.List.Or_error.fold_left primitives ~init:p ~f:(fun p' (header, f) ->
      register_abstraction p' header (Value.Primitive f))
  in
  Util.List.Or_error.fold_left primitive_verb_mappings ~init:p ~f:(fun p' verb_mapping ->
    register_verb_mapping p' verb_mapping)

let rec load_file p ~filepath =
  (* invariant *)
  assert (Filename.is_absolute filepath) ;
  match Map.find p.file_states filepath with
  | Some Loading -> Or_error.error_string "file inclusion dependency cycle"
  | Some Loaded -> Or_error.errorf "file \"%s\" already loaded" filepath
  | None ->
      let p = {p with file_states = Map.add_exn p.file_states ~key:filepath ~data:Loading} in
      let src = In_channel.read_all filepath in
      let%bind stmts = Parser.(parse program src) in
      let%map p =
        Util.List.Or_error.fold_left stmts ~init:p ~f:(fun p' stmt ->
          match stmt with
          | Syntax.Include other_filepath ->
              load_file p' ~filepath:(Filename.concat (Filename.dirname filepath) other_filepath)
          | Syntax.AbstractionDefinition {header; result_identifier; body} ->
              register_abstraction p' header (Value.Block (result_identifier, body))
          | Syntax.Verbs verb_mappings ->
              Util.List.Or_error.fold_left verb_mappings ~init:p' ~f:register_verb_mapping)
      in
      {p with file_states = Map.set p.file_states ~key:filepath ~data:Loaded}