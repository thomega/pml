open Result.Syntax

let cons a alist =
  let* alist in
  Ok (a :: alist)

let cons' a alist =
  let* a and* alist in
  Ok (a :: alist)

let hd err alist =
  let* alist in
  match alist with
  | [] -> Error err
  | a :: _ -> Ok a

let tl err alist =
  let* alist in
  match alist with
  | [] -> Error err
  | _ :: alist -> Ok alist

let rec map f = function
  | [] -> Ok []
  | a :: alist ->
     let* fa = f a
     and* falist = map f alist in
     Ok (fa :: falist)

let%test _ =
  map (fun i -> Ok (10 * i)) [] = Ok []

let%test _ =
  map (fun i -> Ok (10 * i)) [1] = Ok [10]

let%test _ =
  map (fun i -> Ok (10 * i)) [1; 2; 3] = Ok [10; 20; 30]

let rec map' f alist =
  let* alist in
  match alist with
  | [] -> Ok []
  | a :: alist ->
     let* fa = f a
     and* falist = map' f (Ok alist) in
     Ok (fa :: falist)

let%test _ =
  map' (fun i -> Ok (10 * i)) (Ok []) = Ok []

let%test _ =
  map' (fun i -> Ok (10 * i)) (Ok [1]) = Ok [10]

let%test _ =
  map' (fun i -> Ok (10 * i)) (Ok [1; 2; 3]) = Ok [10; 20; 30]
