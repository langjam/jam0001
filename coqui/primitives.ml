open Core
open Syntax
open Or_error.Let_syntax

let lift_int op value =
  match Value.normalize value with
  | Value.LiteralValue (Syntax.Int n) ->
      if Option.is_some n.ordinal then
        Or_error.error_string "cannot perform math on an ordinal number"
      else
        return n.inner
  | _ -> Or_error.errorf "cannot %s a value that is not a number (%s)" op (Value.show value)

let bool_value b = if b then Value.true_ else Value.false_

let is : Value.primitive =
  fun ~program:_ ~env ->
    let x = Map.find_exn env.user_bindings ["x"] in
    let y = Map.find_exn env.user_bindings ["y"] in
    let%map b = Value.equal x y in
    (bool_value b, [])

let matches : Value.primitive =
  fun ~program:_ ~env ->
    let value = Value.normalize @@ Map.find_exn env.user_bindings ["value"] in
    let pattern = Value.normalize @@ Map.find_exn env.user_bindings ["pattern"] in
    let%bind value_list =
      match value with
      | Value.NegativeQuote _ -> Or_error.error_string "cannot use negative quote as value"
      | Value.PositiveQuote vs -> return vs
      | Value.LiteralValue _ 
      | Value.AbstractionValue _ -> return [value]
    in
    match pattern with
    (* | Value.PositiveQuote quote -> *)
    | Value.NegativeQuote quote -> (
        if List.length quote <> List.length value_list then
          return (Value.false_, [])
        else
          match%map
            List.zip_exn quote value_list
            |> Util.List.Or_error.Option.fold_left ~init:[] ~f:(fun captures (q, v) ->
              match q with
              | Value.NegQuoteMatch id ->
                  return (Some ((id, v) :: captures))
              | Value.NegQuoteValue v' ->
                  if%map Value.equal v v' then
                    Some captures
                  else
                    None)
          with
          | Some captures -> (Value.true_, captures)
          | None -> (Value.false_, []))
    | _ -> Or_error.error_string "cannot perform matches against a value that is not a negative quote"

(* TODO: think about how this should work on more arguments than one *)
let apply : Value.primitive =
  fun ~program ~env ->
    let abstraction = Map.find_exn env.user_bindings ["abstraction"] in
    let input = Map.find_exn env.user_bindings ["input"] in
    Interpreter.trace_log env.call_stack "apply abstraction %s\n" (Value.show abstraction) ;
    match abstraction with
    | Value.AbstractionValue (header, body) ->
        let%map (result, _) = Interpreter.apply_abstraction ~program ~header ~body ~inputs:[input] ~call_stack:(Value.LocalAbstraction header :: env.call_stack) in
        (result, [])
    | _ -> Or_error.error_string "cannot apply arguments to something that is not an abstraction"

let int_comparison f : Value.primitive =
  fun ~program:_ ~env ->
    let%bind x = lift_int "compare" @@ Map.find_exn env.user_bindings ["x"] in
    let%map y = lift_int "compare" @@ Map.find_exn env.user_bindings ["y"] in
    (bool_value (f x y), [])

let int_operation f : Value.primitive =
  fun ~program:_ ~env ->
    let%bind x = lift_int "add" @@ Map.find_exn env.user_bindings ["x"] in
    let%map y = lift_int "add" @@ Map.find_exn env.user_bindings ["y"] in
    (Value.LiteralValue (Syntax.Int {ordinal = None; inner = f x y}), [])

let v =
  let word w = AbstractionHeaderWord w in
  let input value = AbstractionHeaderInput {ordinal = None; inner = {article = None; value}} in
  let a_input value = AbstractionHeaderInput {ordinal = None; inner = {article = Some (Indefinite false); value}} in
  let an_input value = AbstractionHeaderInput {ordinal = None; inner = {article = Some (Indefinite true); value}} in
  [ ([input ["x"]; word "is"; input ["y"]], is)
  ; ([a_input ["value"]; word "matches"; a_input ["pattern"]], matches)
  ; ([word "apply"; an_input ["abstraction"]; word "to"; an_input ["input"]], apply)
  ; ([input ["x"]; word "is"; word "less"; word "than"; input ["y"]], int_comparison (<))
  ; ([input ["x"]; word "is"; word "less"; word "than"; word "or"; word "equal"; word "to"; input ["y"]], int_comparison (<=))
  ; ([input ["x"]; word "is"; word "greater"; word "than"; input ["y"]], int_comparison (>))
  ; ([input ["x"]; word "is"; word "greater"; word "than"; word "or"; word "equal"; word "to"; input ["y"]], int_comparison (>=))
  ; ([word "add"; input ["x"]; word "to"; input ["y"]], int_operation (+))
  ; ([word "subtract"; input ["x"]; word "from"; input ["y"]], int_operation (-))
  ; ([word "multiply"; input ["x"]; word "by"; input ["y"]], int_operation ( * )) ]

let verb_mappings =
  let mapping simple progressive = {simple; progressive} in
  [ mapping "apply" "applying"
  ; mapping "add" "adding"
  ; mapping "subtract" "subtracting"
  ; mapping "multiply" "multiplying" ]