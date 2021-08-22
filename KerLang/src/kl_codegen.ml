(** {1 Codegen}

    Code generation module for KerLang
*)

open Kl_parsing
open Kl_2ml
open Kl_constraints
open Kl_IR

let emit_kl_ir (prog : spec list) : (string * ast) list =
  List.fold_left (fun ftable (Spec (_, fname, _) as s) ->
    (* LOG INFO HERE ? *)
    (fname, generate_function s |> compile_function ftable)::ftable
  ) [] prog |> List.rev

let print_info prog =
  List.iter (fun (s, ast) ->
    Format.asprintf "function %s :\n %a" s pp_ast ast
    |> print_endline
  ) prog |> print_newline;
  List.iter (fun (s, ast) -> emit_ast_as_function stdout s ast |> print_newline) prog

let synthetize oc =
  Printf.fprintf oc "
(*---------------------------------------------------------*)\n
(* This code is generated by the KerLang compiler and      *)\n
(* is not intended to be manually edited                   *)\n
(*---------------------------------------------------------*)\n\n\n";
  List.iter (fun (s, ast) ->
    emit_ast_as_function oc s ast |> print_newline
  )