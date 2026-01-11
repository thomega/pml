module type T =
  sig

    type key = string
    type value = string
    (** These type alias are only for documentation.
        Making them abstract would cause too much hassle. *)

    val init : root:string -> (unit, string) result
    (** Initialize a cache. *)

    val get : root:string -> key -> (value option, string) result
    (** Get the contents of a file indexed by a key. *)

    val lookup : root:string -> key -> (key -> (value, string) result) -> (value, string) result
    (** Get the contents of a file indexed by a key.
        If the file is empty, attempt to fill it using the function provided
        and return the result. *)

    val refresh : root:string -> key -> (key -> (value, string) result) -> (unit, string) result
    (** Refresh the contents of a file indexed by a key if the result of the
        function provided differs. *)
    
    val remove : root:string -> key -> (unit, string) result
    (** Remove a file indexed by a key. *)

    val set : root:string -> key -> value -> (unit, string) result
    (** Replace the contents of a file indexed by a key. *)

    val map : root:string -> key -> (value -> (value, string) result)-> (unit, string) result
    (** Change the contents of a file indexed by a key. *)

    val to_alist : root:string -> ((string * string) list, string) result

  end
(** A filesystem based cache for maps from [key] to [value].  Both [key] and [value]
    must be [string]s, because the [key] is a filename and [value] is the raw contents
    of a file.

    The reason for using files is that we want to be able to inspect them easily later.  *)

module type Table =
  sig

    val name : string
    (** Name of a table in the cache.  Currently, this is the name of a
        subdirectory of the cache [root] directory. *)

  end

module Make (_ : Table) : T
