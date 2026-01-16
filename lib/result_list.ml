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
     let* fa = f a and* falist = map f alist in
     Ok (fa :: falist)

let%test _ =
  map (fun i -> Ok (10 * i)) [] = Ok []

let%test _ =
  map (fun i -> Error i) [] = Ok []

let%test _ =
  map (fun i -> Ok (10 * i)) [1] = Ok [10]

let%test _ =
  map (fun i -> Ok (10 * i)) [1; 2; 3] = Ok [10; 20; 30]

let%test _ =
  map (fun i -> if i = 2 then Error i else Ok (10 * i)) [1; 2; 3] = Error 2

let%test _ =
  map (fun i -> if i = 2 || i = 3 then Error i else Ok (10 * i)) [1; 2; 3] = Error 2

let%test _ =
  map (fun i -> if i = 7 then Error i else Ok (10 * i)) [1; 2; 3] = Ok [10; 20; 30]

let rec fold_left' f acc = function
  | [] -> acc
  | a :: alist ->
     let* acc in
     let fa = f acc a in
     fold_left' f fa alist

let fold_left f acc alist =
  fold_left' f (Ok acc) alist

let%test _ =
  fold_left (fun acc i -> Ok ((10 * i) :: acc)) [] [] = Ok []

let%test _ =
  fold_left (fun _acc i -> Error i) [] [] = Ok []

let%test _ =
  fold_left (fun acc i -> Ok ((10 * i) :: acc)) [] [1] = Ok [10]

let%test _ =
  fold_left (fun acc i -> Ok ((10 * i) :: acc)) [] [1; 2; 3] = Ok [30; 20; 10]

let%test _ =
  fold_left (fun acc i -> if i = 2 || i = 3 then Error i else Ok ((10 * i) :: acc)) [] [1; 2; 3] =
    Error 2

let rec fold_right' f alist acc =
  match alist with
  | [] -> acc
  | a :: alist ->
     let* acc = fold_right' f alist acc in
     f a acc

let fold_right f alist acc =
  fold_right' f alist (Ok acc)

let%test _ =
  fold_right (fun i acc -> Ok ((10 * i) :: acc)) [] [] = Ok []

let%test _ =
  fold_left (fun i _acc -> Error i) [] [] = Ok []

let%test _ =
  fold_right (fun i acc -> Ok ((10 * i) :: acc)) [1] [] = Ok [10]

let%test _ =
  fold_right (fun i acc -> Ok ((10 * i) :: acc)) [1; 2; 3] [] = Ok [10; 20; 30]

let%test _ =
  fold_right (fun i acc -> if i = 2 || i = 3 then Error i else Ok ((10 * i) :: acc)) [1; 2; 3] [] =
    Error 3
