(** Compute the disc identifier(s) used by thr MusicBrainz database. *)

type t =
  { id : string; (** The disc identifier used by MusicBrainz.
                     It is a 28 character BASE64 string
                     made URL safe by replacing ['+'], ['/']
                     and the padding character ['='] with
                     ['.'], ['_'] and ['-'] respectively. *)
    freedb : string (** The disc identifier use by FreeDB. (obsolete) *);
    toc : string; (** The TOC of the disc, used for fuzzy searches.
                     The spaces are already replaced by ['+']. *)
    submission_url : string (** URL for submitting a diskid to MusicBrainz. *)
  }
(** See the {{: https://musicbrainz.org/doc/Disc_ID_Calculation }MusicBrainz documentation}
    for details. *)

val get : ?device:string -> unit -> (t, string) result
(** Get the disc identifiers from the CD in [device].
    The default for [device] is taken from [default_device ()]. *)

val default_device : unit -> string
(** Usually ["/dev/cdrom"]. *)
