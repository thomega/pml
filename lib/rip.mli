(** Ripping and tagging. *)

val script : Tagged.t -> (unit, string) result
(** Write a shell script for ripping, encoding and tagging. *)
