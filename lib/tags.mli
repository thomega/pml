(** WIP: tags and file system layout for musical works ripped from disc(s). *)

module Artist : sig
  type t =
    { name : string (** The [sort_name] if available, accept [name] as substitute. *) ;
      artist_type : Artist_type.t;
      lifespan : Lifespan.t;
      id : string }
  val of_mb : Musicbrainz.Artist.t -> t
end

val disjoint_oldest_opt : Artist.t list -> (Artist.t * Artist.t list) option
(** Check if there is an oldest artist in the list, who died before any
    of the others where born.  This artist must be the composer.

    {e One could think of generalizing this to the case that there are
       more artists that are older than the rest.  This would allow
       for collaborative compositions. However, this appears to be too
       rare to be worth the effort.}*)

module All_tracks : sig
  type t =
    { title : string;
      composers : Artist.t list;
      performers : Artist.t list;
      tracks : int }
  val of_mb : unit -> t
end

module Track : sig
  type t =
    { number : int;  (** Overall position of the track in the whole work, counting from 1. *)
      title : string;
      performers : Artist.t list;
      inherited : All_tracks.t;
      id : string }
  val of_mb : Musicbrainz.Track.t -> t
end

module Partial : sig
  type t =
    { release : string; (** The Musicbrainz id of the release from which the data are taken. *)
      disc : string; (** The discid from which the audio was ripped. *)
      title : string; (** The title of the whole work. This can not be empty. *)
      composers : Artist.t list; (** Composers of a work that is expected to be performed
                                     by others.  This will usually be left empty for
                                     popular music.  *)
      performers : Artist.t list; (** Instrumentalists, singers, conductors. *)
      tracks : Track.t list;
      total_tracks : int (** The total number of tracks of the piece.  This is only needed
                             for the correct number of leading zeros in numbers in filenames. *)
    }
end
(** The part of a a musical work that fits on one disc. *)
