(** All we know about a disc/medium. *)

type t =
  { title : string option;
    tracks : Track.t list;
    id : string }

val of_mb : Mb_medium.t -> t


