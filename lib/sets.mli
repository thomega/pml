(** Ubiquitous sets of MBID (i.e. UUID) strings *)

module MBID : Set.S with type elt = string

val mbid_union : MBID.t list -> MBID.t
(** Shorthand for the union of a list of sets. *)
