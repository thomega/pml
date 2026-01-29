(** All we know about a release. *)

type t =
  { title : string option; (** The title of the whole work. *)
    artists : Artist.Collection.t; (** Composers of a work that is expected to be performed
                                       by others.  This will usually be left empty for
                                       popular music.
                                       Performers: instrumentalists, singers, conductors, etc. *)
    media : Medium.t list;
    id : string }

val of_mb : Mb_release.t -> t


