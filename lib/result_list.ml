open Result.Syntax

let cons a alist =
  let* a and* alist in
  Ok (a :: alist)

let hd err = function
  | Error _ as e -> e
  | Ok [] -> Error err
  | Ok (a :: _) -> Ok a

let tl err = function
  | Error _ as e -> e
  | Ok [] -> Error err
  | Ok (_ :: alist) -> Ok alist

let rec map f = function
  | Error _ as e -> e
  | Ok [] -> Ok []
  | Ok (a :: alist) ->
     let* fa = f a
     and* falist = map f (Ok alist) in
     Ok (fa :: falist)

let%test _ =
  map (fun i -> Ok (10 * i)) (Ok []) = Ok []

let%test _ =
  map (fun i -> Ok (10 * i)) (Ok [1]) = Ok [10]

let%test _ =
  map (fun i -> Ok (10 * i)) (Ok [1; 2; 3]) = Ok [10; 20; 30]
