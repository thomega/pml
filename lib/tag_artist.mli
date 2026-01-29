(** All we know about an artist. *)

type t =
  { name : string; (** The [sort_name] if available, accept [name] as substitute. *)
    artist_type : Artist_type.t; (** Determines the credit order. *)
    lifespan : Lifespan.t; (** Can separate composers from performers. *)
    id : string (** MBID *) }

val compare : t -> t -> int
(** Compare artists by [artist_type] and use [lifespan] and [name] as a tie breakers. *)

val of_mb : Mb_artist.t -> t
(** Translate and replace [None] by defaults.  *)

val of_name : string -> t
(** Make up a name, without further information. *)

module Collection : Set.S with type elt = t

val lifespan_gaps : Collection.t -> Collection.t list
(** Check if there is are artists, who died before others where born.
    Such artists must be the composer(s). *)

val of_credits : Mb_artist_credit.t list -> Collection.t
(** Follow MusicBrainz' indirections. *)
