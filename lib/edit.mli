(** Edit strings. *)

val common_prefix : string list -> string * string list
(** Find the longest common prefix in a list of string
    and return it together with the tails. *)

val blank_to_none : string option -> string option
(** Map blank strings to [None]. *)
