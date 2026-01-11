module type T =
  sig

    val init : root:string -> (unit, string) result
    (** Initialize a cache. *)

    val get : root:string -> string -> (string option, string) result
    (** Get the contents of a file indexed by a key. *)

    val remove : root:string -> string -> (unit, string) result
    (** Remove a file indexed by a key. *)

    val set : root:string -> string -> string -> (unit, string) result
    (** Replace the contents of a file indexed by a key. *)

    val map : root:string -> string -> (string -> (string, string) result)-> (unit, string) result
    (** Change the contents of a file indexed by a key. *)

  end

module type Table =
  sig

    val name : string
    (** Name of a table in the cache.  Currently, this is the name of a
        subdirectory of the cache [root] directory. *)

  end

module Make (_ : Table) : T
