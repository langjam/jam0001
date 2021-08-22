module List = struct

  module Or_error = struct
    open Core.Or_error.Let_syntax

    let fold_left ls ~init ~f =
      Core.List.fold_left ls ~init:(return init) ~f:(fun acc el ->
        let%bind acc' = acc in
        f acc' el)

    let fold_right ls ~init ~f =
      Core.List.fold_right ls ~init:(return init) ~f:(fun el acc ->
        let%bind acc' = acc in
        f el acc')

    let map ls ~f =
      fold_right ls ~init:[] ~f:(fun h t ->
        let%map h' = f h in
        h' :: t)

    let rec for_all ls ~f =
      match ls with
      | [] -> return true
      | h :: t ->
          let%bind b = f h in
          if b then for_all t ~f else return false

    module Option = struct
      let fold_left ls ~init ~f =
        fold_left ls ~init:(Some init) ~f:(fun acc ->
          match acc with
          | Some acc' -> f acc'
          | None -> Core.Fn.const (return None))
    end
  end
end