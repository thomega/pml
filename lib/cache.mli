module type T =
  sig

    val lookup : root:string -> string -> (string option, string) result
    (** Look up the contents of a file indexed by a key. *)

    val delete : root:string -> string -> (unit, string) result
    (** Remove a file indexed by a key. *)

    val replace : root:string -> string -> string -> (unit, string) result
    (** Replace the contents of a file indexed by a key. *)

  end

module type Table =
  sig
    val name : string
    (** Name of a table in the cache. *)
  end

module Make (_ : Table) : T
