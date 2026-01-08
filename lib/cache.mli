val lookup : root:string -> subdir:string -> string -> (string option, string) result
(** Look up the contents of a file indexed by a key. *)

val replace : root:string -> subdir:string -> string -> string -> (unit, string) result
(** Replace the contents of a file indexed by a key. *)

val delete : root:string -> subdir:string -> string -> (unit, string) result
(** Remove a file indexed by a key. *)
