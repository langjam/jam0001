
type ('a, 'b) schema = 'a list -> ('b * 'a list) option

let check (pred : 'a -> bool) : ('a, 'a) schema = function
  | [] -> None
  | x::xs ->
    if pred x then Some (x, xs) else None

let (>>=) (schm : ('a, 'b) schema) (f : 'b -> ('a, 'c) schema) : ('a, 'c) schema = fun input ->
  match schm input with
  | None -> None
  | Some (res, next) -> f res next

let (let*) = (>>=)

let return value = fun input -> Some (value, input)

let (<$>) (f : 'b -> 'c) (schm : ('a, 'b) schema) : ('a, 'c) schema =
  let* res = schm in
  return (f res)

let (<*>) (sc1 : ('a, 'b -> 'c) schema) (sc2 : ('a, 'b) schema) : ('a, 'c) schema =
  let* f = sc1 in
  let* x = sc2 in
  return (f x)

let ( *> ) (sc1 : ('a, 'b) schema) (sc2 : ('a, 'c) schema) : ('a, 'c) schema =
  let* _ = sc1 in sc2

let ( <* ) (sc1 : ('a, 'b) schema) (sc2 : ('a , 'c) schema) : ('a, 'b) schema =
  let* res = sc1 in
  let* _ = sc2 in
  return res

let (<|>) (sc1 : ('a, 'b) schema) (sc2 : ('a, 'b) schema) : ('a, 'b) schema = fun input ->
  match sc1 input with
  | None -> sc2 input
  | Some _ as res -> res

let (|||) t1 t2 = fun x -> (t1 x) || (t2 x)

let sc_subject = check ((=) "this" ||| (=) "it")

let sc_verb = check ((=) "is" ||| (=) "has" ||| (=) "returns")

let sc_deter = check ((=) "a" ||| (=) "the" ||| (=) "some")

let sc_noun = check ((=) "house" ||| (=) "product" ||| (=) "car")

let sc_object = (fun x y -> String.concat " " [x; y]) <$> sc_deter <*> sc_noun

let sc_verb = (fun x y z -> (x, y, z)) <$> (sc_object <|> sc_subject) <*> (sc_verb) <*> sc_object

let test = sc_verb ["it"; "has"; "a"; "product"]