(** {1 Codegen}

    Code generation module for KerLang
*)

open Kl_parsing
open Kl_constraints
open Kl_IR

let emit_kl_ir (prog : spec list) : (string * ast) list =
  List.fold_left (fun ftable (Spec (_, fname, _) as s) ->
      Printf.printf "\n[Processing function \x1b[1;36m%s\x1b[0m]\n" fname;
      let res = (fname, generate_function ftable s |> compile_function ftable)::ftable in
      Printf.printf "[\x1b[1;32mOk\x1b[0m]\n";
      res
    ) [] prog |> List.rev

module type TermRealizer = sig
  (** Realize an anonymous term *)
  val realize_term : out_channel -> ast -> unit

  (** Realize a named declaration *)
  val realize_decl : out_channel -> string -> ast -> unit

  (** Realize a header containing a comment and maybe some helper functions *)
  val realize_header : out_channel -> unit

  (** Realize an automatic call to the "main" function (provided it exists) *)
  val realize_entrypoint_call : out_channel -> unit
end

(** Type of kl_IR realizers *)
module Realizer (X : TermRealizer) = struct
  include X
  let realize oc (prog_list : (string * ast) list) =
    let has_main =
      List.exists (fun (name, prog) ->
          let is_main = (name = "main") in
          if is_main then begin
            let main_params_count = ast_count_params prog in
            if main_params_count <> 0 then begin
              Kl_errors.error "the main function must not take or use any parameter"
            end
          end;
          is_main
        ) prog_list in
    realize_header oc;
    List.iter (fun (name, prog) -> realize_decl oc name prog) prog_list;
    if has_main then realize_entrypoint_call oc
end

module ML_Realizer = Realizer (struct
    let realize_header oc =
      Kl_2ml.emit_header oc

    let realize_term oc prog =
      Kl_2ml.emit_ast_as_function oc prog

    let realize_decl oc name prog =
      Kl_2ml.emit_ast_as_function_decl oc name prog

    let realize_entrypoint_call oc =
      Kl_2ml.emit_entrypoint_call oc
  end)

module PY_Realizer = Realizer (struct
    let realize_header oc =
      Kl_2py.emit_header oc

    let realize_term oc prog =
      Kl_2py.emit_ast oc prog

    let realize_decl oc name prog =
      Kl_2py.emit_ast_as_function_decl oc name prog

    let realize_entrypoint_call oc =
      Kl_2py.emit_entrypoint_call oc
  end)

module C_Realizer = Realizer (struct
    let realize_header oc =
      Kl_2c.emit_header oc

    let realize_term oc prog =
      Kl_2c.emit_ast oc prog

    let realize_decl oc name prog =
      Kl_2c.emit_ast_as_function_decl oc name prog

    let realize_entrypoint_call oc =
      Kl_2c.emit_entrypoint_call oc
  end)

type lang = ML | PY | C

let pp_lang fmt = function
  | ML -> Printf.fprintf fmt "OCaml"
  | PY -> Printf.fprintf fmt "Python"
  | C -> Printf.fprintf fmt "C"

let realize oc lang prog =
  Printf.printf "[Realizing the program in \x1b[1;36m%a\x1b[0m]\n" pp_lang lang;
  match lang with
  | ML ->
    ML_Realizer.realize oc prog
  | PY ->
    PY_Realizer.realize oc prog
  | C -> C_Realizer.realize oc prog

let executes prog =
  Kl_IR.flookup "main" prog
  |> Kl_IR.eval [] prog