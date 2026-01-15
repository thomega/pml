(** Inspired by [Stdlib.Result.Syntax] *)

val ( let* ) : 'a option -> ('a -> 'b option) -> 'b option
val ( and* ) : 'a option -> 'b option -> ('a * 'b) option
(** Monadic *)

val ( let+ ) : 'a option -> ('a -> 'b) -> 'b option
val ( and+ ) : 'a option -> 'b option -> ('a * 'b) option
(** Applicative *)
