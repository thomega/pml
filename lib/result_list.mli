(** Maps, folds and filters for [Result.t]. *)

val map : ('a -> ('b, 'e) result) -> ('a list, 'e) result -> ('b list, 'e) result
(** Like [List.map] but with early exit error handling.
    {e Not} tail recursive and not even tail-mod-cons,
    because we must inspect the results. *)
