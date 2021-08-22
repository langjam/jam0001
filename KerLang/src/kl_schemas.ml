(** {1 Schemas}

    Using a parser combinator approach to do NLP stuff
*)

(** A [('a, 'b)] schema is a chunk of code that analyses a sequence of ['a]
    and produces a ['b] *)
type ('a, 'b) schema = 'a list -> ('b * 'a list) option

(** [check pred] is a simple [('a, 'a)] schema that detect an ['a] satisfying [pred]
    and returns it *)
let check (pred : 'a -> bool) : ('a, 'a) schema = function
  | [] -> None
  | x::xs ->
    if pred x then Some (x, xs) else None

(** [let* x = schema1 in schema2] is a schema perform the [schema1] analysis and then the [schema2]
    analysis (where [schema2] may depend on [x]) *)
let (let*) (schm : ('a, 'b) schema) (f : 'b -> ('a, 'c) schema) : ('a, 'c) schema = fun input ->
  match schm input with
  | None -> None
  | Some (res, next) -> f res next

(** Trivial schema *)
let return value = fun input -> Some (value, input)

(** Map schema *)
let (<$>) (f : 'b -> 'c) (schm : ('a, 'b) schema) : ('a, 'c) schema =
  let* res = schm in
  return (f res)

(** Application schema *)
let (<*>) (sc1 : ('a, 'b -> 'c) schema) (sc2 : ('a, 'b) schema) : ('a, 'c) schema =
  let* f = sc1 in
  let* x = sc2 in
  return (f x)

(** Ignore left schema *)
let ( *> ) (sc1 : ('a, 'b) schema) (sc2 : ('a, 'c) schema) : ('a, 'c) schema =
  let* _ = sc1 in sc2

(** Ignore right schema *)
let ( <* ) (sc1 : ('a, 'b) schema) (sc2 : ('a , 'c) schema) : ('a, 'b) schema =
  let* res = sc1 in
  let* _ = sc2 in
  return res

(** Choice schema *)
let (<|>) (sc1 : ('a, 'b) schema) (sc2 : ('a, 'b) schema) : ('a, 'b) schema = fun input ->
  match sc1 input with
  | None -> sc2 input
  | Some _ as res -> res

(** Predicate disjunction *)
let (|||) t1 t2 = fun x -> (t1 x) || (t2 x)

(** Predicate conjunction *)
let (&&&) t1 t2 = fun x -> (t1 x) && (t2 x)

(** {2 Some NLP related schemas} *)

let sc_subject = check ((=) "this" ||| (=) "it")

let sc_verb = check ((=) "is" ||| (=) "has" ||| (=) "returns")

let sc_deter = check ((=) "a" ||| (=) "the" ||| (=) "some")

let sc_noun = check ((=) "house" ||| (=) "product" ||| (=) "car")

let sc_object = (fun x y -> String.concat " " [x; y]) <$> sc_deter <*> sc_noun

let sc_verb = (fun x y z -> (x, y, z)) <$> (sc_object <|> sc_subject) <*> (sc_verb) <*> sc_object

let test = sc_verb ["it"; "has"; "a"; "product"]