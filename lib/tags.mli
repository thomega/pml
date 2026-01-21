(** WIP: tags and file system layout for musical works ripped from disc(s). *)

module Artist : sig
  type t =
    { name : string; (** The [sort_name] if available, accept [name] as substitute. *)
      artist_type : Artist_type.t; (** Determines the credit order. *)
      lifespan : Lifespan.t; (** Can separate composers from performers. *)
      id : string (** MBID *) }
  val compare : t -> t -> int
  val of_mb : Musicbrainz.Artist.t -> t
end

module Artists : Set.S with type elt = Artist.t

val lifespan_gaps : Artists.t -> Artists.t list
(** Check if there is are artists, who died before others where born.
    Such artists must be the composer(s). *)

module All_tracks : sig
  type t =
    { title : string;
      composers : Artists.t;
      performers : Artists.t;
      tracks : int }
  val of_mb : unit -> t
end

module Track : sig
  type t =
    { number : int;  (** Overall position of the track in the whole work, counting from 1. *)
      title : string;
      composers : Artists.t;
      performers : Artists.t;
      all_tracks : All_tracks.t;
      id : string }
  val of_mb : Musicbrainz.Track.t -> t
end

module Partial : sig
  type t =
    { release : string; (** The Musicbrainz id of the release from which the data are taken. *)
      disc : string; (** The discid from which the audio was ripped. *)
      title : string; (** The title of the whole work. This can not be empty. *)
      composers : Artists.t; (** Composers of a work that is expected to be performed
                                 by others.  This will usually be left empty for
                                 popular music.  *)
      performers : Artists.t; (** Instrumentalists, singers, conductors. *)
      tracks : Track.t list;
      total_tracks : int (** The total number of tracks of the piece.  This is only needed
                             for the correct number of leading zeros in numbers in filenames. *)
    }
end
(** The part of a a musical work that fits on one disc. *)
