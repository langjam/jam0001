
(*
open Rg
open Kl_IR

let hash_string (s: string): int32 =
  let x = ref (Int32.of_int 8) in
  for i = 0 to (String.length s)-1 do
    let char_code = Int32.of_int (Char.code (s.[i])) in
    x := Int32.logxor !x char_code;
    x := Int32.shift_left !x 3;
    x := Int32.add !x char_code
  done;
  !x

let shit_leaf (rg: rg_t): rg_t * ast =
  let rg: rg_t = ref (hash_string name) in
  let is_var: bool = (let (new_rg, value) = rg_int (0, 2) !rg in rg := new_rg; value = 0) in
  let value: int = (let (new_rg, value) = rg_int (0, 3) !rg in rg := new_rg; value = 0) in
  if is_var then
    Var(value)
  else
    Cst(value)

let rec shit_ast_rec (rg: rg_t): rg_t * ast =
  let rg: rg_t = ref (hash_string name) in
  let is_var: bool = (let (new_rg, value) = rg_int (0, 2) !rg in rg := new_rg; value = 0) in
  let value: int = (let (new_rg, value) = rg_int (0, 3) !rg in rg := new_rg; value = 0) in
  if is_var then
    Var(value)
  else
    Cst(value)

let shit_ast (name: string): ast =
  let rg: rg_t = ref (hash_string name) in
  let can_use_out: bool = (let (new_rg, value) = rg_int (0, 9) !rg in rg := new_rg; value = 0) in
  let depth_max: int = (let (new_rg, value) = rg_int (2, 4) !rg in rg := new_rg; value) in


help im useless
*)
