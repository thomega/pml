module Taggable : sig

  type t =
    { medium : Mb_medium.t;
      release : Mb_release.t;
      discid : string }

  val of_discid : ?medium:string -> root:string -> string -> (t, string) result
  (** Find the released disc matching the discid. *)

  val print : t -> unit
  (** Exploration, WIP ... *)

end

