(** WIP: tags and file system layout for musical works ripped from disc(s). *)

(** The data structures in [Musicbrains] are a slightly embellished translation
    of the JSON response for the [release] containing a particular disc.
    Currently, the only embellishment is the [Lifespan.t] of the artists.

    Therefore, we have a hierarchy
    {v
     release
      - title
      - artist_credit*
         - name
         - artist
      - medium*
         - position
         - title
         - track*
            - position
            - title
            - artist_credit*
               - name
               - artist
            - recording?
               - title
               - artist_credit*
                  - name
                  - artist
     v}
    and we have to take care of two things for tagging the rip of a disk
    {ul {- partially invert the hierarchy to put a reference to the release
           into the medium}
        {- disambiguate the titles and artists of release, medium, track
           and recording.}}
 *)

module Artist : sig

  type t =
    { name : string; (** The [sort_name] if available, accept [name] as substitute. *)
      artist_type : Artist_type.t; (** Determines the credit order. *)
      lifespan : Lifespan.t; (** Can separate composers from performers. *)
      id : string (** MBID *) }

  val compare : t -> t -> int
  (** Compare artists by [artist_type] and use [lifespan] and [name] as a tie breakers. *)

  val of_mb : Musicbrainz.Artist.t -> t
  (** Translate and replace [None] by defaults.  *)

end

module Artists : Set.S with type elt = Artist.t

val lifespan_gaps : Artists.t -> Artists.t list
(** Check if there is are artists, who died before others where born.
    Such artists must be the composer(s). *)

module Track : sig
  type t =
    { number : int;  (** Overall position of the track in the whole work, counting from 1. *)
      title : string;
      recording_title : string option;
      artists : Artists.t;
      id : string }
  val of_mb : Musicbrainz.Track.t -> t
end

module Medium : sig
  type t =
    { title : string option;
      tracks : Track.t list;
      id : string }
  val of_mb : Musicbrainz.Medium.t -> t
end

module Release : sig
  type t =
    { title : string option; (** The title of the whole work. *)
      artists : Artists.t; (** Composers of a work that is expected to be performed
                               by others.  This will usually be left empty for
                               popular music.
                               Performers: instrumentalists, singers, conductors, etc. *)
      media : Medium.t list;
      id : string }
  val of_mb : Musicbrainz.Release.t -> t
end

module Disc : sig

  type title =
    | User of string (** User selected. *)
    | Tracks of string (** Longest common prefix of the tracks *)
    | Medium of string (** Disc. *)
    | Release of string (** Release. *)
  (** The origin of the title. *)

  type t =
    { composer : Artist.t option; (** The primary sorting key for the ripped files.
                                      From this, we will derive the name of the top
                                      level directory for storing the files.
                                      For classical music, this will be the composer.
                                      For popular music, it will be the top billed
                                      performer. *) 
      titles : title list; (** Possible titles of the work.
                               From this and the [performer], if present,
                               we will derive the name of the second level
                               directory for storing the files. *)
      performer : Artist.t option; (** The top billed performer for classical music, 
                                       to distinguish different interpretations.
                                       Empty for popular music. *)
      artists : Artists.t;
      tracks : Track.t list;
      tracks_orig : Track.t list option; (** The tracks with the original names iff a common
                                             prefix has been stripped to be used as title. *)
      total_tracks : int; (** The total number of tracks of the release containing the disc.
                              This is only needed for the correct number of leading zeros in
                              numbers in filenames. *)
      discid : string; (** The discid from which the audio was ripped. *)
      medium_id : string;
      release_id : string
    }

  val of_mb : Musicbrainz.Taggable.t -> t

  val user_title : string -> t -> (t, string) result
  (** (Interactively?) edit the tags. *)

  val user_composer : string -> t -> (t, string) result
  (** (Interactively?) name composer. *)

  val user_performer : string -> t -> (t, string) result
  (** (Interactively?) name top billed performer. *)

  val script : t -> (unit, string) result
  (** Write a shell script for ripping, encoding and tagging. *)

  val print : t -> unit
  (** Exploration, WIP ... *)

end

