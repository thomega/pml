module type Error =
  sig

    type t = { error : string; help : string option }
    (** On Error, Musicbrainz returns and error message *)

    val get_error_opt : string -> string option
    (** Check if the JSON contains an top level ["error"] element.
        Ignores parsing errors.  They must be handled subsequently. *)

  end
(** JSON error response from Musicbrainz. *)

module Error : Error
(** Use [Error.get_error_opt] to test if the response contains
    and ["error"] element with a message. *)

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

module type Cached =
  sig

    val get : root:string -> string -> (string, string) result
    (** Return the JSON for the given key, preferring the cache located at [root].
        If there is no local entry for the key, it will be created from the
        result of the remote lookup. *)

    val local : root:string -> string -> (string option, string) result
    (** Return the JSON for the given key, using only the cache located at [root]. *)

    val remote : string -> (string, string) result
    (** Return the JSON for the given key, ignoring any cache. *)

    val all_local : root:string -> ((string * string) list, string) result
    (** Return the cached [key] JSON pairs. *)

    val url : string -> (string, string) result
    (** Return the URL for querying Musicbrainz for the entry corresponding to a key. *)

    module Internal : Cache.T with type key = string and type value = string
    (** Access the public interface of the [Cache.T] used to implement
        this cached table. *)

  end
(** Query the Musicbrainz database with local caching to avoid excessive network
    traffic when finetuning the tagging of tracks. *)

module Discid_cached : Cached
(** Find information stored about a disc, in particular the releases. *)

val releases_of_discid : root:string -> string -> (string list, string) result
(** There can be more than one release of a given disc.  Return
    them as a list of MBID/UUID strings. *)

module Release_cached : Cached
(** Find information stored about a release: tracks, artists, etc. *)

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

module Disc : sig
  type t =
    { id : string (** While this is optional in the DTD, it should be there anyway. *);
      ignored : Jsont.json }
end

module Medium : sig
  type t =
    { id : string (** While this is optional in the DTD, it should be there anyway. *);
      position : int option;
      title : string option;
      discs : Disc.t list; (** Is this relevant for us?  There are no titles or credits. *)
      tracks : Track.t list;
      ignored : Jsont.json }
  end

module Release : sig
  type t =
    { id : string (** While this is optional in the DTD, it should be there anyway. *);
      title : string option;
      artist_credit : Artist_Credit.t list;
      media : Medium.t list }
  end

val media_of_discid : root:string -> string -> (Medium.t list, string) result
(** Find the release matching the diskid. *)

