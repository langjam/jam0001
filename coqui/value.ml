open Core

module Id = struct
  module T = struct
    type t = string list
    [@@deriving compare, equal, sexp]
  end

  include T
  include Comparable.Make (T)
end

type abstraction_call =
  | GlobalAbstraction of AbstractionSignature.t
  | LocalAbstraction of Syntax.abstraction_header

let show_abstraction_call = function
  | GlobalAbstraction signature -> Printf.sprintf "[%s]" (AbstractionSignature.show signature)
  | LocalAbstraction id -> Printf.sprintf "#(%s)" (Syntax.show_abstraction_header id)

let pp_abstraction_call fmt = Fn.compose (Format.pp_print_string fmt) show_abstraction_call

type file_state =
  | Loading
  | Loaded
[@@deriving show]

type t =
  | LiteralValue of Syntax.literal
  | AbstractionValue of Syntax.abstraction_header * abstraction_body
  | NegativeQuote of negative_quote_element list
  | PositiveQuote of t list
and negative_quote_element =
  | NegQuoteValue of t
  | NegQuoteMatch of Syntax.identifier
and env =
  { user_bindings: t Id.Map.t [@opaque]
  ; context: t list
    (* for debugging purposes *)
  ; call_stack: abstraction_call list }
and abstraction_body =
  | Primitive of primitive
  | Block of Syntax.identifier * Syntax.block
  | Phrase of Syntax.phrase
and primitive = program:program -> env:env -> (t * (Syntax.identifier * t) list) Or_error.t [@opaque]
and program =
  { file_states: file_state String.Map.t [@opaque]
  ; abstractions: (Syntax.abstraction_header * abstraction_body) AbstractionSignature.Map.t [@opaque]
  ; mapped_verbs: String.Set.t [@opaque] (* simple forms of registered verbs *)
  ; verb_mappings: string String.Map.t [@opaque] (* progressive -> simple *) }
[@@deriving show]

let empty_env = {user_bindings = Id.Map.empty; context = []; call_stack = []}
let rec normalize = function
  | NegativeQuote q ->
      if List.for_all q ~f:(function NegQuoteValue _ -> true | _ -> false) then
        let vs = List.map q ~f:(function NegQuoteValue v -> v | _ -> failwith "cannot get here") in
        normalize (PositiveQuote vs)
      else
        NegativeQuote (List.map q ~f:(function NegQuoteValue v -> NegQuoteValue (normalize v) | x -> x))
  | PositiveQuote [v] -> normalize v
  | v -> v

let rec pretty_to_string value =
  let wrap_quote s = "^{" ^ String.concat ~sep:" " s ^ "}" in
  match value with
  | LiteralValue (Word w) -> w
  | LiteralValue (Int n) -> Int.to_string n.inner ^ Option.value_map n.ordinal ~f:Syntax.string_of_ordinal ~default:""
  | AbstractionValue _ -> "<abstraction>"
  | PositiveQuote ls -> wrap_quote (List.map ls ~f:pretty_to_string)
  | NegativeQuote ls ->
    wrap_quote (List.map ls ~f:(function
      | NegQuoteValue v -> pretty_to_string v
      | NegQuoteMatch id -> "%(" ^ String.concat ~sep:" " id.value ^ ")"))

let rec equal x y =
  let open Or_error.Let_syntax in
  match (normalize x, normalize y) with
  | (LiteralValue l1, LiteralValue l2) -> return (Syntax.equal_literal l1 l2)
  | (PositiveQuote xs, PositiveQuote ys) ->
      if List.length xs = List.length ys then
        Util.List.Or_error.for_all (List.zip_exn xs ys) ~f:(fun (x', y') -> equal x' y')
      else
        return false
  | (AbstractionValue _, _)
  | (_, AbstractionValue _) -> Or_error.error_string "cannot compare abstractions for equality"
  | (NegativeQuote _, _)
  | (_, NegativeQuote _) -> Or_error.error_string "cannot compare negative quotes for equality"
  | _ -> return false

let true_ = LiteralValue (Word "true")
let false_ = LiteralValue (Word "false")