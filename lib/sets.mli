(* sets.mli -- part of PML (Physical Media Library)

  Copyright (C) 2026 by Thorsten Ohl <ohl@physik.uni-wuerzburg.de>

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <https://www.gnu.org/licenses/>. *)

(** Sets of things. *)

module MBID : Set.S with type elt = string
(** Ubiquitous sets of MBID (i.e. UUID) strings *)

val mbid_union : MBID.t list -> MBID.t
(** Shorthand for the union of a list of sets. *)

module Integers : sig

  module S : Set.S with type elt = int

  val of_string : string -> (S.t, string) result
  (** Parse expressions like [1,3-5,7]. *)

  val to_string : S.t -> string
  (** For error messages. *)

end
(** Integers. *)
