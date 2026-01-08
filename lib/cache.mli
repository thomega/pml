type config =
  { root : string; (** The root of the directory hierarchy. *)
    subdir : string (** A subdirectory of [root]. *);
    prefix : string (** Prefixed to the key. *);
    suffix : string (** Suffixed to the key. *)
  }

val lookup : config -> string -> (string option, string) result
(** Look up the contents of a file indexed by a key. *)

val replace : config -> string -> string -> (unit, string) result
(** Replace the contents of a file indexed by a key. *)

val delete : config -> string -> (unit, string) result
(** Remove a file indexed by a key. *)
