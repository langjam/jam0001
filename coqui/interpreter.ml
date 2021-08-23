open Core
open Value

open Or_error.Let_syntax

(* uncomment when debugging the interpreter *)
(* let trace_log call_stack =
  Printf.ksprintf (fun str ->
    (* TODO: reindent str *)
    let indent = String.make (max 0 (2 * (List.length call_stack - 1))) ' ' in
    let header =
      List.hd call_stack
      |> Option.value_map ~default:"" ~f:(fun call -> Value.show_abstraction_call call ^ " ")
    in
    let str' = String.substr_replace_all str ~pattern:"\n" ~with_:("\n  " ^ indent) in
    Printf.printf "%s* %s%s\n" indent header str') *)

let trace_log _call_stack = Printf.ksprintf ignore

let evaluate_reference ~env =
  function
  | Syntax.LocalBinding id ->
      Map.find env.user_bindings id.value
      |> Result.of_option ~error:(Error.createf "binding not found: %s" (Syntax.show_identifier id))
  | Syntax.SingularContext ->
      List.hd env.context
      |> Result.of_option ~error:(Error.createf "cannot access signular context when no context exists")
  | Syntax.PluralContext -> (
      match List.length env.context with
      | 0 -> Or_error.error_string "cannot access plural context when there are no contexts"
      | 1 -> Or_error.error_string "cannot access plural context when there is only one context"
      | _ ->
        List.fold_left env.context ~init:(LiteralValue (Syntax.Word "nothing")) ~f:(fun acc value ->
          PositiveQuote [value; LiteralValue (Syntax.Word "and"); acc])
        |> return)

(* Evaluate a phrase containing a single non-literal phrase segment. *)
let rec evaluate_phrase ~program ~env phrase =
  match List.filter phrase ~f:(function Syntax.PhraseLiteral (Syntax.Word _) | Syntax.Escape _ -> false | _ -> true) with
  | [segment] -> evaluate_phrase_segment ~program ~env segment
  | [] -> Or_error.error_string "cannot evaluate phrase that does not contain any literal phrase segments"
  | _ ->
      Printf.printf "!!! %s\n" (Syntax.show_phrase phrase) ;
      Or_error.error_string "cannot evaluate phrase that contains more than one non-literal phrase segment"
(* Evaluate a non-literal phrase segment. *)
and evaluate_phrase_segment ~program ~env =
  function
  | Syntax.PhraseLiteral (Word _) -> Or_error.error_string "cannot evaluate literal word phrase segment"
  | Syntax.PhraseLiteral (Int n) -> return (LiteralValue (Int n))
  (* | Syntax.Escape block -> evaluate_block_result ~program ~env block *)
  | Syntax.Escape _ -> Or_error.error_string "cannot evaluate block phrase segment"
  | Syntax.PhraseReference ref -> evaluate_reference ~env ref.inner
  | Syntax.Abstraction (header, body) -> return (AbstractionValue (header, Phrase body))
  | Syntax.Quote quote -> evaluate_quote_exp ~program ~env quote
  | Syntax.Math math_exp -> evaluate_math_exp ~program ~env math_exp.inner
  | Syntax.Call phrase ->
      let%map (result, _) = call_phrase ~program ~env phrase.inner in
      result
(* Evaluate a quote to a value. *)
and evaluate_quote_exp ~program ~env quote_exp =
  let%map segments = Util.List.Or_error.map quote_exp ~f:(evaluate_quote_exp_segment ~program ~env) in
  let is_positive = List.for_all segments ~f:(function `Value _ -> true | `Match _ -> false) in
  if is_positive then
    PositiveQuote (List.map segments ~f:(function
      | `Value v -> v
      | _ -> failwith "internal error: malformed positive quote"))
  else
    NegativeQuote (List.map segments ~f:(function
      | `Value v -> NegQuoteValue v
      | `Match id -> NegQuoteMatch id))
and evaluate_quote_exp_segment ~program ~env =
  function
  | Syntax.Match id -> return (`Match id)
  | Syntax.QuotePhraseSegment (PhraseLiteral lit) -> return (`Value (LiteralValue lit))
  | Syntax.QuotePhraseSegment segment ->
      let%map value = evaluate_phrase_segment ~program ~env segment in
      `Value value
and evaluate_math_exp ~program ~env math_exp =
  let lift_int value =
    match normalize value with
    | LiteralValue (Int n) ->
        if Option.is_some n.ordinal then
          Or_error.error_string "cannot perform math on ordinal number"
        else
          return n.inner
    | _ -> Or_error.error_string "cannot evaluate math expressions on non-integer values"
  in
  let int_op f l r =
    let%bind n = evaluate_math_exp ~program ~env l >>= lift_int in
    let%map m = evaluate_math_exp ~program ~env r >>= lift_int in
    LiteralValue (Int {ordinal = None; inner = f n m})
  in
  match math_exp with
  | Syntax.MathPhraseSegment (PhraseLiteral (Word w)) ->
      Or_error.errorf "cannot evaluate word literal inside a math expression: \"%s\"" w
  | Syntax.MathPhraseSegment segment -> evaluate_phrase_segment ~program ~env segment
  | Syntax.Add (l, r) -> int_op (+) l r
  | Syntax.Sub (l, r) -> int_op (-) l r
  | Syntax.Mul (l, r) -> int_op ( * ) l r
(* Evaluate a block that binds a result. *)
and evaluate_block_result ~program ~env ~result_identifier block =
  let%bind final_env = evaluate_block ~program ~env block in
  Map.find final_env.user_bindings result_identifier.Syntax.value
  |> Result.of_option ~error:(Error.of_string "block did not bind a result during evaluation")
and evaluate_block ~program ~env block =
  Util.List.Or_error.fold_left block ~init:env ~f:(fun env' -> evaluate_block_segment ~program ~env:env')
(* Evaluate a block segment. *)
and evaluate_block_segment ~program ~env =
  function
  | Syntax.BlockLiteral _
  | Syntax.BlockReference _ -> return env
  | Syntax.Intro (id, phrase) -> (
      trace_log env.call_stack "INTRODUCING %s\n" (Syntax.show_identifier id) ;
      let%bind result = evaluate_phrase ~program ~env phrase in
      match Map.add env.user_bindings ~key:id.value ~data:result with
      | `Ok user_bindings -> return {env with user_bindings}
      | `Duplicate -> Or_error.errorf "duplicate bindings defined in block: %s" (Syntax.show_identifier id))
  | Syntax.ContextIntro phrase ->
      let%map (result, env') = call_phrase ~program ~env phrase in
      {env' with context = result :: env.context}
  | Syntax.Ternary (cond_phrase, _, then_block, _, else_block) ->
      let%bind (cond, env') = call_phrase ~program ~env cond_phrase in
      let%bind block =
        match Value.normalize cond with
        | LiteralValue (Syntax.Word "true") -> return then_block
        | LiteralValue (Syntax.Word "false") -> return else_block
        | v -> Or_error.errorf "expected boolean, got \"%s\"" (Value.show v)
      in
      evaluate_block ~program ~env:env' block
  | Syntax.Exception str ->
      Or_error.errorf "program raised an exception: %s" str
(* Call an abstraction from a phrase. *)
and call_phrase ~program ~env phrase =
  let%bind signature = AbstractionSignature.of_phrase ~verb_mappings:program.verb_mappings phrase.value in
  let%bind (header, body) =
    Map.find program.abstractions signature
    |> Result.of_option ~error:(Error.createf "abstraction not found: %s" (AbstractionSignature.show signature))
  in
  (* TODO: prove to myself that this is safe *)
  let%bind inputs =
    phrase.value
    |> List.filter ~f:(function Syntax.PhraseLiteral (Syntax.Word _) -> false | _ -> true)
    |> Util.List.Or_error.map ~f:(evaluate_phrase_segment ~program ~env)
  in
  let%bind (result, new_bindings) = apply_abstraction ~program ~header ~body ~inputs ~call_stack:(GlobalAbstraction signature :: env.call_stack) in
  let%map user_bindings =
    Util.List.Or_error.fold_left new_bindings ~init:env.user_bindings ~f:(fun user_bindings (id, value) ->
      match Map.add user_bindings ~key:id.Syntax.value ~data:value with
      | `Ok user_bindings' -> return user_bindings'
      | `Duplicate -> Or_error.errorf "cannot define same binding twice in a block: %s" (Syntax.show_identifier id))
  in
  (result, {env with user_bindings})
(* Apply an abstraction from it's components and arguments. *)
and apply_abstraction ~program ~header ~body ~inputs ~call_stack =
  trace_log call_stack "ENTER" ;
  let input_ids = List.filter_map header ~f:(function Syntax.AbstractionHeaderInput i -> Some i | _ -> None) in
  (* invariant *)
  assert (List.length inputs = List.length input_ids) ;
  let local_user_bindings =
    inputs
       (* strip shallow ordinals *)
    |> List.map ~f:(function
      | LiteralValue (Int n) -> LiteralValue (Int {n with ordinal = None})
      | v -> v)
    |> List.zip_exn input_ids
    |> List.fold_left ~init:Id.Map.empty ~f:(fun new_user_bindings (id, input) ->
      Map.add_exn new_user_bindings ~key:id.inner.value ~data:input)
  in
  let env = {user_bindings = local_user_bindings; context = []; call_stack} in
  let%map r =
    match body with
    | Primitive f -> f ~program ~env
    | Block (result_identifier, block) ->
        let%map result = evaluate_block_result ~program ~env ~result_identifier block in
        (result, [])
    | Phrase phrase ->
        let%map result = evaluate_phrase ~program ~env phrase in
        (result, [])
  in
  trace_log call_stack "EXIT" ;
  r