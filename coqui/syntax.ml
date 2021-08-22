open Core

type article =
  | Definite (* the *)
  | Indefinite of bool (* a, an *)
[@@deriving show]
type 'a articulated =
  { article: article option
  ; value: 'a }
[@@deriving show]

type ordinal = Th | St | Nd | Rd
[@@deriving equal, show]
type 'a ordinaled =
  { ordinal: ordinal option
  ; inner: 'a }
[@@deriving equal, show]

let string_of_ordinal = function
  | Th -> "th"
  | St -> "st"
  | Nd -> "nd"
  | Rd -> "rd"

let rec ordinal_of_int = function
  | n when n < 0 -> ordinal_of_int (-n)
  | 0 -> Th
  | 1 -> St
  | 2 -> Nd
  | 3 -> Rd
  | n when n < 20 -> Th
  | n ->
      if n mod 10 = 0 then
        Th
      else
        ordinal_of_int (n mod 10)

type identifier = string list articulated
[@@deriving show]

type reference =
  | SingularContext
  | PluralContext
  | LocalBinding of identifier
[@@deriving show]

type arithmetic_operation =
  | Add
  | Subtract
  | Multiply
[@@deriving show]

type literal =
  | Word of string [@equal String.equal]
  | Int of int ordinaled [@equal Int.equal]
[@@deriving equal, show]

type abstraction_header_segment =
  | AbstractionHeaderWord of string
  | AbstractionHeaderInput of identifier ordinaled
[@@deriving show]
type abstraction_header = abstraction_header_segment list
[@@deriving show]

type phrase = phrase_segment list
and phrase_segment =
  | PhraseLiteral of literal
  | PhraseReference of reference ordinaled
  | Abstraction of abstraction_header * phrase (* TODO combine abstraction_header with phrase_header? *)
  | Quote of quote_exp
  | Math of math_exp ordinaled
  | Call of phrase articulated ordinaled
  | Escape of block
and quote_exp = quote_exp_segment list (* there are positive and negative quotes, but they are not differentiated at a syntactic level currently*)
and quote_exp_segment =
  | Match of identifier
  | QuotePhraseSegment of phrase_segment
and math_exp =
  | Add of math_exp * math_exp
  | Sub of math_exp * math_exp
  | Mul of math_exp * math_exp
    (* not exactly perfect, but it will do for now... *)
    (* eg: blocks, quotes, and abstractions don't really make sense here *)
  | MathPhraseSegment of phrase_segment
and block = block_segment list
and block_segment =
  | BlockLiteral of literal
  | BlockReference of reference
  | Intro of identifier * phrase
  | ContextIntro of phrase articulated
  | Ternary of phrase articulated * literal list * block * literal list * block
  | Exception of string (* TODO: support blocks and interpolation here*)
[@@deriving show]

type verb_mapping =
  { simple: string
  ; progressive: string }
[@@deriving show]

type statement = 
  | Include of string
  | AbstractionDefinition of
      { header: abstraction_header 
      ; result_identifier: identifier 
      ; body: block }
  | Verbs of verb_mapping list
[@@deriving show]