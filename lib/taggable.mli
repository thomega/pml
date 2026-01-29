(** MusicBrainz records for a medium together with its release.*)

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
