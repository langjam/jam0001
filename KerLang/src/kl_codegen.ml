(** {1 Codegen}

    Code generation module for KerLang
*)

open Kl_parsing
open Kl_constraints
open Kl_IR

let emit_kl_ir (prog : spec list) : (string * ast) list =
  List.map (fun (Spec (_, fname, _) as s) ->
    (* LOG INFO HERE ? *)
    fname, generate_function s |> compile_function []
  ) prog

let print_prog =
  List.iter (fun (s, ast) ->
    Format.asprintf "function %s :\n %a" s pp_ast ast
    |> print_endline
  )