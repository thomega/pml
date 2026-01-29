(** All we know about a track. *)

type t =
  { number : int;  (** Overall position of the track in the whole work, counting from 1. *)
    number_on_disc : int;
    title : string;
    recording_title : string option;
    artists : Artist.Collection.t;
    id : string }

val of_mb : Mb_track.t -> t

val recording_title : t -> t
(** If [recording_title] exists, replace [title] by it and set it to [None]. *)
