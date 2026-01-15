(** Query the MusicBrainz API. *)

type api
(** Options defining a web API. *)

val musicbrainz : api
(** The web API for lookups in the MusicBrainz data base. *)

type query =
  { table : string;
    inc : string list }
(** The MusicBrainz table to be queried and the additional
    information to be included in the result.*)

val exec : api -> query -> string -> (string, string) result
(** Perform a lookup for a key in the MusicBrainz database table
    specified by the [query].  If successful, the result is a
    JSON string. *)

val url : api -> query -> string -> string
(** Return the URL corresponding to the above query.
    This can be pasted into a web browser for debugging. *)

val curl : ?timeout:int -> user_agent:string -> string -> (string, string) result
(** Query a url directly. For debugging. *)




