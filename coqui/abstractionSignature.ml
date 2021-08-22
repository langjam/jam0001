open Core
open Syntax

type segment =
  | AbstractionWord of string
  | AbstractionInput of {has_ordinal: bool}
[@@deriving compare, equal, sexp]

let show_segment = function
  | AbstractionWord w -> w
  | AbstractionInput {has_ordinal} -> "_" ^ if has_ordinal then "th" else ""

let pp_segment fmt = Fn.compose (Format.pp_print_string fmt) show_segment

module T = struct
  type t = segment list
  [@@deriving compare, equal, sexp]
end

include T
include Comparable.Make (T)

let show signature =
  signature
  |> List.map ~f:show_segment
  |> String.concat ~sep:" "

let pp fmt = Fn.compose (Format.pp_print_string fmt) show

let normalize_word w = String.lowercase w

let unroll_from_segments segments ~lift_literal =
  let (acc, signature) =
    List.fold_left segments ~init:([], []) ~f:(fun (acc, signature) segment ->
      match lift_literal segment with
      | Some w -> (normalize_word w :: acc, signature)
      | None -> ([], List.rev acc :: signature))
  in
  List.rev (List.rev acc :: signature)

let rewrite_progressive_verb ~verb_mappings word =
  String.Map.find verb_mappings word
  |> Option.value ~default:word

let of_abstraction_header header =
  List.map header ~f:(function
    | AbstractionHeaderWord w -> AbstractionWord (normalize_word w)
    | AbstractionHeaderInput ord_id -> AbstractionInput {has_ordinal = Option.is_some ord_id.ordinal})

let of_phrase ~verb_mappings phrase =
  let open Or_error in
  let input (type a) (ord : a ordinaled) = AbstractionInput {has_ordinal = Option.is_some ord.ordinal} in
  Util.List.Or_error.map phrase ~f:(function
    | PhraseLiteral (Word w) -> return (AbstractionWord (w |> normalize_word |> rewrite_progressive_verb ~verb_mappings))
    | PhraseLiteral (Int n) -> return (input n)
    | PhraseReference r -> return (input r)
    | Math m -> return (input m)
    | Call p -> return (input p)
    | Abstraction _
    | Quote _ -> return (AbstractionInput {has_ordinal = false})
    | Escape _ -> error_string "cannot put escape inside of a call phrase")