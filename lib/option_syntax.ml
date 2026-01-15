let product o1 o2 =
  match o1, o2 with
  | Some a1, Some a2 -> Some (a1, a2)
  | _, _ -> None

let ( let* ) = Option.bind
let ( and* ) = product

let ( let+ ) r f = Option.map f r
let ( and+ ) = product

let%test _ =
  match
    let* a = Some 1
    and* b = Some 2 in
    Some (a + b)
  with
  | Some 3 -> true
  | _ -> false

let%test _ =
  match
    let* a = None
    and* b = Some 2 in
    Some (a + b)
  with
  | None -> true
  | _ -> false

let%test _ =
  match
    let* a = Some 1
    and* b = None in
    Some (a + b)
  with
  | None -> true
  | _ -> false

let%test _ =
  match
    let* a = None
    and* b = None in
    Some (a + b)
  with
  | None -> true
  | _ -> false

let%test _ =
  match
    let* a = Some 1 in
    let* a = Some (a + 2) in
    Some a
  with
  | Some 3 -> true
  | _ -> false

let%test _ =
  match
    let* a = None in
    let* a = Some (a + 2) in
    Some a
  with
  | None -> true
  | _ -> false

let%test _ =
  match
    let* _ = Some 1 in
    let* a = None in
    Some a
  with
  | None -> true
  | _ -> false

let%test _ =
  match
    let* _ = None in
    let* a = None in
    Some a
  with
  | None -> true
  | _ -> false

let%test _ =
  match
    let+ a = Some 1
    and+ b = Some 2 in
    a + b
  with
  | Some 3 -> true
  | _ -> false

let%test _ =
  match
    let+ a = Some 1
    and+ b = None in
    a + b
  with
  | None -> true
  | _ -> false

let%test _ =
  match
    let+ a = None
    and+ b = Some 2 in
    a + b
  with
  | None -> true
  | _ -> false

let%test _ =
  match
    let+ a = None
    and+ b = None in
    a + b
  with
  | None -> true
  | _ -> false

