open Angstrom
open Core

open Syntax

module Pred = struct
  let either p q x = p x || q x

  let is_alpha = function
    | 'a' .. 'z'
    | 'A' .. 'Z' -> true
    | _          -> false

  let is_punctuation = function
    | '.' | ','
    | ':' | ';'
    | '\'' | '"'
    | '?'  | '!' -> true
    | _          -> false

  let is_numeric = function
    | '0' .. '9' -> true
    | _          -> false

  let is_whitespace = function
    | ' '
    | '\t'
    | '\n' -> true
    | _    -> false
end

module Util = struct
  let __debug_indent_level = ref 0

  (* uncomment when debugging the parser *)
  (* let __debug name p =
    let print_indent s =
      Printf.printf "%s%s\n" (String.make (!__debug_indent_level * 2) ' ') s
    in
    let fmt_peek_char = Option.value_map ~f:(Printf.sprintf ";\"%c\"") ~default:"" in
    let enter_debug l c =
      print_indent (Printf.sprintf "CHECK %s (%d%s)" name l (fmt_peek_char c))
      (* incr __debug_indent_level *)
    in
    let exit_debug l c =
      (* decr __debug_indent_level ; *)
      print_indent (Printf.sprintf "DONE %s (%d%s)" name l (fmt_peek_char c))
    in
    (lift2 enter_debug pos peek_char) *> (p <?> name) <* (lift2 exit_debug pos peek_char) *)
  
  let __debug name p = p <?> name

  let skip_while1 p = satisfy p *> skip_while p

  let maybe p = option None (p >>| Option.some)

  let drop p = p *> return ()

  let ( **> ) p q = p *> skip_while Pred.is_whitespace *> q
  let ( <** ) p q = p <* skip_while Pred.is_whitespace <* q
  let ( *!> ) p q = p *> skip_while1 Pred.is_whitespace *> q
  let ( <!* ) p q = p <* skip_while1 Pred.is_whitespace <* q

  let infix f i p q =
    lift2
      f
      (p <** i)
      (commit **> q)

  let infix_precedence_chain (p : 'a t) (precedences : ('b t * ('a -> 'a -> 'c)) list list) : 'c t =
    List.fold_right precedences ~init:p ~f:(fun ops acc ->
      fix (fun layer ->
        let op_chains = List.map ops ~f:(fun (op_p, f) -> op_p **> (layer >>| Fn.flip f)) in
        lift2
          (fun l f -> f l)
          (acc <** return ())
          (choice (op_chains @ [return Fn.id]))))
end

open Util

let ws = skip_while Pred.is_whitespace
let ws1 = skip_while1 Pred.is_whitespace

let quoted_string : string t =
  char '"'
  *> many_till (choice [string "\\\"" *> return '"'; any_char]) (char '"')
  >>| String.of_char_list

let articulated (p : 'a t) : 'a articulated t =
  lift2
    (fun article value -> {article; value})
    (maybe
      (choice
        (* should be `not alpha?`*)
        [ string_ci "the" *> ws1 *> return Definite
        ; string_ci "an" *> ws1 *> return (Indefinite true)
        ; string_ci "a" *> ws1 *> return (Indefinite false) ]))
    p

let any_ordinal : ordinal t =
  choice
    [ string "th" *> return Th
    ; string "st" *> return St
    ; string "nd" *> return Nd
    ; string "rd" *> return Rd ]

let indirect_ordinaled (p : 'a t) : 'a ordinaled t =
  lift2
    (fun inner ordinal -> {ordinal; inner})
    p
    (maybe (string "th" *> ws1 *> return Th))

let word : string t =
  let english_word =
    let alpha = satisfy Pred.is_alpha >>| String.of_char in
    lift2
      (fun h t -> String.concat ~sep:"" (h :: t))
      alpha
      (many (choice [lift2 (^) (string "-") alpha; alpha]))
  in
  (* TODO: disambiguate properly (negative numbers, for example) *)
  let punctuation =
    let* p =
      choice
        [ string "::" *> return None
        ; satisfy Pred.is_punctuation >>| Option.some ]
    in
    match p with
    | None -> fail "punctuation rejected"
    | Some c -> return (String.of_char c)
  in
  choice [english_word; punctuation]

let int : int ordinaled t =
  let* negative = option false (char '-' *> return true) in
  let* num_str = take_while1 Pred.is_numeric in
  let* ordinal = maybe any_ordinal <|> (satisfy (fun c -> not (Pred.is_alpha c)) *> return None) in
  let pos_number = Int.of_string num_str in
  let number = if negative then -pos_number else pos_number in
  let r = {ordinal; inner = number} in
  match ordinal with
  | Some o ->
      if not (Syntax.equal_ordinal (ordinal_of_int number) o) then
        fail "invalid ordinal number"
      else
        return r
  | None ->
      return r

let literal : literal t =
  __debug "literal" @@
  choice
    [ word >>| (fun w -> Word w)
    ; int >>| (fun n -> Int n) ]

let identifier : identifier t =
  articulated (sep_by1 ws word)

let abstraction_header : abstraction_header t =
  let abstraction_header_segment : abstraction_header_segment t =
    __debug "abstraction_header_segment" @@
    choice
      [ word >>| (fun t -> AbstractionHeaderWord t)
      ; indirect_ordinaled (char '%' *> char '(' **> identifier <** char ')') >>| (fun id -> AbstractionHeaderInput id)
      ; char '%' *> word >>| (fun w -> AbstractionHeaderInput {ordinal = None; inner = {article = None; value = [w]}}) ]
  in
  __debug "abstraction_header" @@ sep_by1 ws abstraction_header_segment

let reference =
  choice
    [ string_ci "@it" *> return SingularContext
    ; string_ci "@them" *> return PluralContext
    ; char '$' *> word >>| (fun w -> LocalBinding {article= None; value= [w]})
    ; char '$' *> char '(' *> identifier <* char ')' >>| (fun id -> LocalBinding id) ]

let phrase' (block : block t) : phrase t =
  fix (fun phrase ->
    let phrase_segment : phrase_segment t =
      fix (fun phrase_segment ->
        let abstraction =
          __debug "abstraction" @@
          both
            (char '#' *> char '{' **> abstraction_header)
            (ws *> string "::" **> phrase <** char '}')
        in
        let quote =
          let quote_segment =
            __debug "quote_segment" @@
            choice
              [ char '%' *> char '(' *> identifier <* char ')' >>| (fun id -> Match id)
              ; char '%' *> word >>| (fun w -> Match {article = None; value = [w]})
              ; phrase_segment >>| (fun ps -> QuotePhraseSegment ps) ]
          in
          __debug "quote" @@
          choice
            [ char '^' *> char '{' **> sep_by1 ws quote_segment <** char '}'
            ; char '^' *> literal >>| (fun l -> [QuotePhraseSegment (PhraseLiteral l)]) ]
        in
        let math_exp =
          __debug "math_exp" @@
          infix_precedence_chain (phrase_segment >>| (fun ps -> MathPhraseSegment ps))
            [ [ (char '*', (fun l r -> Mul (l, r))) ]
            ; [ (char '+', (fun l r -> Add (l, r)))
              ; (char '-', (fun l r -> Sub (l, r))) ] ]
        in
        __debug "phrase_segment" @@
        choice
          [ literal >>| (fun l -> PhraseLiteral l)
          ; indirect_ordinaled reference >>| (fun r -> PhraseReference r)
          ; abstraction >>| (fun (h, b) -> Abstraction (h, b))
          ; quote >>| (fun q -> Quote q)
          ; indirect_ordinaled (char '&' *> char '[' **> math_exp <** char ']') >>| (fun m -> Math m)
          ; indirect_ordinaled (char '[' **> articulated phrase <** char ']') >>| (fun p -> Call p)
          ; char '{' **> block <** char '}' >>| (fun b -> Escape b) ])
      in
      sep_by1 ws phrase_segment)

let block : block t =
  fix (fun block ->
    let block_segment : block_segment t =
      let ternary =
        (* TODO: ?(IDENTIFIER :: block) *)
        let+ cond = char '?' *> char '[' *> commit **> articulated (phrase' block) <** char ']'
        and+ then_text = ws *> sep_by ws literal <* ws
        and+ then_ = char '{' **> block <** char '}'
        and+ else_text = ws *> sep_by ws literal <* ws
        and+ else_ = char '{' **> block <** char '}' in
        Ternary (cond, then_text, then_, else_text, else_)
      in
      __debug "block_segment" @@
      choice
        [ char '%' *> commit *> char '(' **>
            lift2
              (fun id block -> Intro (id, block))
              (identifier <** (__debug "intro.1.::" @@ string "::") <** return ())
              (phrase' block)
          <** char ')'
        ; ternary
        ; char '@' *> char '[' **> articulated (phrase' block) <** char ']' >>| (fun p -> ContextIntro p)
        ; char '!' *> char '(' *> many_till any_char (char ')') >>| (fun cs -> Exception (String.of_char_list cs))
        ; reference >>| (fun r -> BlockReference r)
        ; literal >>| (fun l -> BlockLiteral l) ]
    in
    __debug "block" @@
    sep_by1 ws block_segment)

let phrase = phrase' block

let statement : statement t =
  let phrase_header = __debug "phrase_header" @@ char '[' **> abstraction_header <** char ']' in
  let definition =
    __debug "definition" @@
    lift3
      (fun header result_identifier body -> AbstractionDefinition {header; result_identifier; body})
      (string "define" *> commit **>  phrase_header <* ws)
      ( (string "resulting" *!> string "in" **> char '[' **> identifier <** char ']' <* ws)
        <|> return {article = Some (Indefinite false); value = ["result"]} )
      (string "as" **> char '{' **> block <** char '}')
  in
  let verb_mapping =
    __debug "verb_mapping" @@
    lift2
      (fun simple progressive -> {simple = String.lowercase simple; progressive = String.lowercase progressive})
      (char '[' **> string "to" *!> word <** string "->" <** return ())
      (word <** char ']')
  in
  __debug "statement" @@
  choice
    [ definition
    ; string "include" *> commit **> quoted_string >>| (fun src -> Include src)
    ; string "verbs" *> commit **> char '{' **> sep_by1 ws verb_mapping <** char '}' >>| (fun vs -> Verbs vs) ]

let program : statement list t =
  sep_by1 ws1 statement <* end_of_input

let parse p str =
  let open Buffered in
  let open Or_error in
  let fail unconsumed stack error =
    let width = 12 in
    let recent_input =
      Bigstring.sub unconsumed.buf
        ~pos:(max (unconsumed.off - width) 0)
        ~len:(min unconsumed.len width)
      |> Bigstring.to_string
    in
    let upcoming_input = 
      Bigstring.sub unconsumed.buf
        ~pos:unconsumed.off
        ~len:(min unconsumed.len width)
      |> Bigstring.to_string
    in
    let error_msg =
      Printf.sprintf "parse error (stack = [%s]; error = \"%s\"; recent_input = \"%s\"; upcoming_input = \"%s\")"
        (String.concat ~sep:", " stack)
        error
        recent_input
        upcoming_input
    in
    error_string error_msg
  in
  match feed (parse p) (`String str) with
  | Partial f -> (
    match f `Eof with
    | Done (_unconumed, output) ->
        return output
    | Partial _f ->
        error_string "parser did not stop at end of input"
    | Fail (unconsumed, stack, error) ->
        fail unconsumed stack error)
  | Done (_unconsumed, _result) ->
      error_string "parser terminated early"
  | Fail (unconsumed, stack, error) ->
      fail unconsumed stack error