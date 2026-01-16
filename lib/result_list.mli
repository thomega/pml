(** Maps, folds and filters for [Result.t]. *)

val cons : 'a -> ('a list, 'e) result -> ('a list, 'e) result
val cons' : ('a, 'e) result -> ('a list, 'e) result -> ('a list, 'e) result
val hd : 'e -> ('a list, 'e) result -> ('a, 'e) result
val tl : 'e -> ('a list, 'e) result -> ('a list, 'e) result

val map : ('a -> ('b, 'e) result) -> 'a list -> ('b list, 'e) result
(** Like [List.map] but with early exit error handling.
    {e Not} tail recursive and not even tail-mod-cons,
    because we must inspect the results. *)

(** Note that a 
    [val map' : ('a -> ('b, 'e) result) -> ('a list, 'e) result -> ('b list, 'e) result]
    would just be syntactic sugar for
    {v
     let map' f alist =
       let* alist in
       map f alist
     v} *)

val fold_left : ('a -> 'b -> ('a, 'e) result) -> 'a -> 'b list -> ('a, 'e) result
(** Like [List.fold_left] but with early exit error handling. *)
