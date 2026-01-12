module type T =
  sig

    type key
    (** While [key] will always be [string] in out application,
        this adds additional type checks. *)

    type value
    (** While [value] will almost always be [string] in out application,
        this adds additional type checks.  One could think of switching to
        parsed JSON, but we need to store flat files anyway. *)

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

    val to_alist : root:string -> ((key * value) list, string) result

  end
(** A filesystem based cache for maps from [key] to [value].  Both [key] and [value]
    must be [string]s, because the [key] is a filename and [value] is the raw contents
    of a file.

    The reason for using files is that we want to be able to inspect them easily later.
    Therefore, we assume that *)

module type Table =
  sig

    val name : string
    (** Name of a table in the cache.  Currently, this is the name of a
        subdirectory of the cache [root] directory. *)

    type key
    val key_of_string : string -> (key, string) result
    val key_to_string : key -> (string, string) result
    (** This want to can enforce constraints on the keys and their
        textual representations.  Therefore, we must allow for the
        case that the translation functions fail. *)

    type value
    val value_of_string : string -> (value, string) result
    val value_to_string : value -> (string, string) result
    (** This want to can enforce constraints on the values and their
        textual representations.  Therefore, we must allow for the
        case that the translation functions fail. *)

  end
(** Specification of a cache table, [key] and [value] types and the required
    translation functions from and to strings. *)

module Make (T : Table) : T with type key = T.key and type value = T.value
