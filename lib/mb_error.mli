(** JSON error response from Musicbrainz.
    Use [Error.get_error_opt] to test if the response contains
    an [error] element with a message. *)

type t = private { error : string; help : string option }
(** On Error, Musicbrainz returns and error message *)

val get_error_opt : string -> string option
(** Check if the JSON contains a top level [error] element.
    Ignores parsing errors.  They must be handled subsequently. *)

