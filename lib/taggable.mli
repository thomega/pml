(** MusicBrainz records for a medium together with its release.*)

(** The data structures in [Mb_release] are a slightly embellished translation
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
           into the medium.}
        {- disambiguate the titles and artists of release, medium, track
           and recording.}}
 *)

type t =
  { medium : Mb_medium.t;
    release : Mb_release.t;
    discid : string }

val of_discid : ?medium:string -> root:string -> string -> (t, string) result
(** Find the released disc matching the discid.
    Behind the scenes, it also inserts the extended [artist] records
    from the cache and updates the latter from MusicBrainz, if necessary. *)

val print : t -> unit
(** Exploration, WIP ... *)
