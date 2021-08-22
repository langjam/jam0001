(** {1 Codegen}

    Code generation module for KerLang
*)

open Kl_parsing
open Kl_2ml
open Kl_constraints
open Kl_IR

let emit_kl_ir (prog : spec list) : (string * ast) list =
  List.fold_left (fun ftable (Spec (_, fname, _) as s) ->
      (fname, generate_function s |> compile_function ftable)::ftable
    ) [] prog |> List.rev

let print_info prog =
  List.iter (fun (s, ast) ->
      Format.asprintf "function %s :\n %a" s pp_ast ast
      |> print_endline
    ) prog |> print_newline;
  List.iter (fun (name, ast) -> emit_ast_as_function_decl stdout name ast |> print_newline) prog

module type TermRealizer = sig
  (** Realize an anonymous term *)
  val realize_term : out_channel -> ast -> unit

  (** Realize a named declaration *)
  val realize_decl : out_channel -> string -> ast -> unit

  val realize_header : out_channel -> unit
end

(** Type of kl_IR realizers *)
module Realizer (X : TermRealizer) = struct
  include X
  let realize oc =
    realize_header oc;
    List.iter (fun (name, code) -> realize_decl oc name code)
end

module ML_Realizer = Realizer (struct
    let realize_header oc =
      Kl_2ml.emit_header oc

    let realize_term oc prog =
      Kl_2ml.emit_ast oc prog

    let realize_decl oc name prog =
      Kl_2ml.emit_ast_as_function_decl oc name prog
  end)

module PY_Realizer = Realizer (struct
    let realize_header oc =
      Kl_2py.emit_header oc

    let realize_term oc prog =
      Kl_2py.emit_ast oc prog

    let realize_decl oc name prog =
      Kl_2py.emit_ast_as_function_decl oc name prog
  end)

type lang = ML | PY | C

let realize oc lang prog =
  match lang with
  | ML -> ML_Realizer.realize oc prog
  | PY -> PY_Realizer.realize oc prog
  | _ -> assert false