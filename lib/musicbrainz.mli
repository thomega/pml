module Discid_cache : Cache.T with type key = string and type value = string
(** For testing only.  Will be removed from the final API. *)

module Release_cache : Cache.T with type key = string and type value = string
(** For testing only.  Will be removed from the final API. *)

val get_discid_direct : string -> (string, string) result
(** Return the JSON for the given discid, ignoring any cache. *)

val get_release_direct : string -> (string, string) result
(** Return the JSON for the given release, ignoring any cache. *)

val get_discid_from_cache : root:string -> string -> (string option, string) result
(** Return the JSON for the given discid, using only the cache located at [root]
    if possible. *)

val get_release_from_cache : root:string -> string -> (string option, string) result
(** Return the JSON for the given release, using only the cache located at [root]
    if possible. *)

val get_discid_cached : root:string -> string -> (string, string) result
(** Return the JSON for the given discid, preferring the cache located at [root]
    if possible. *)

val get_release_cached : root:string -> string -> (string, string) result
(** Return the JSON for the given release, preferring the cache located at [root]
    if possible. *)

val get_cached_discids : root:string -> ((string * string) list, string) result
val get_cached_releases : root:string -> ((string * string) list, string) result

module type Raw =
  sig

    val normalize : string -> (string, string) result
    (** Decode and encode the JSON in the string by [Jsont] to obtain
        a canonical representation. *)

    val of_file : string -> (Jsont.json, string) result
    (** Decode a JSON stored in a file. *)

    val print_file : string -> unit
    (** Decode a JSON stored in a file, encode it and write it to
        standard output. *)

    val dump_schema_file : string -> unit
    (** Decode a JSON stored in a file and write the structure to
        standard output. *)

  end
(** Process the JSON files returned by MusicBrainz uninterpreted.
    NB: except [normalize], all functions are for debugging only. *)

module Raw : Raw

val releases_of_discid : root:string -> string -> (string list, string) result

module Artist : sig
  type t =
    { id : string; (** The MBID (i.e. UUID) of the artist.  While this is
                       declared as optional in the DTD, we can safely assume
                       that is always there. *)
      name : string option; (** The common name of the artist. *)
      sort_name : string option; (** The name in last name, first name order
                                     for persons and with articles removed
                                     from ensemble names. *)
      artist_type : string option; (** {e Is there an exhaustive list?} *)
      disambiguation : string option; (** {e Is there an exhaustive list?} *)
      ignored : Jsont.json (** Currently ignored JSON elements. *)
    }
end

module Artist_Credit : sig
  type t =
    { name : string option;
      artist : Artist.t option;
      ignored : Jsont.json }
end

module Recording : sig
  type t =
    { id : string (** While this is optional in the DTD, it should be there anyway. *);
      title : string option;
      artist_credit : Artist_Credit.t list;
      ignored : Jsont.json }
end

module Track : sig
  type t =
    { id : string (** While this is optional in the DTD, it should be there anyway. *);
      position : int option;
      title : string option;
      artist_credit : Artist_Credit.t list;
      recording : Recording.t option;
      ignored : Jsont.json }
end
