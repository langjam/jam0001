(** Deterministic random number generator *)

(** Linear congruential generator state. *)
type lcg_t = int32

let i32 (x: int) = Int32.of_int x
let add32 (x: int32) (y: int32) = Int32.add x y
let mul32 (x: int32) (y: int32) = Int32.mul x y
let mod32 (x: int32) (y: int32) = Int32.rem x y
let rsh32 (x: int32) (y: int) = Int32.shift_right x y
let lsh32 (x: int32) (y: int) = Int32.shift_left x y
let xor32 (x: int32) (y: int32) = Int32.logxor x y

let lcg_helf_iter (lcg: lcg_t): lcg_t =
  (*
    GNU libc parameters, see the table at
    https://en.wikipedia.org/wiki/Linear_congruential_generator#Parameters_in_common_use
   although rand is probably not just a LCG.
  *)
  (mod32 (add32 (mul32 lcg (i32 1103515245)) (i32 12345)) (i32 2147483648))

(** Randomp number generator state. *)
type rg_t = lcg_t

let rg_step (rg: rg_t): rg_t * int32 =
  let a = lcg_helf_iter rg in
  let b = lcg_helf_iter a in
  (b, (xor32 (lsh32 (rsh32 a 16) 16) (rsh32 b 16)))

(** Returns a random number in the range inf_sup using rg,
    and returns the new state of rg. *)
let rg_int (inf_sup: int * int) (rg: rg_t): rg_t * int =
  let (rg, x) = rg_step rg in
  let x: int = Int32.to_int x in
  let (inf, sup) = inf_sup in
  let x = inf + (abs (x mod (sup - inf + 1))) in
  (rg, x)

(** Prints n lines of random numbers in the range inf_sup using rg.
    Returns the new state of rg. *)
let rg_test_print (inf_sup: int * int) (n: int) (rg: rg_t): rg_t =
  let rg_ref = ref rg in
  for _ = 0 to n-1 do
    for _ = 0 to 9 do
      let (new_rg, x) = rg_int inf_sup !rg_ref in
      rg_ref:= new_rg;
      print_int x;
      print_string "\t";
    done;
    print_string "\n";
  done;
  !rg_ref