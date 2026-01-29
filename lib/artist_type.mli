(* artist_type.mli -- part of PML (Physical Media Library)

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

(** Classify artists for tagging. *)

type voice =
  | Soprano
  | Mezzo
  | Alto
  | Counter
  | Tenor
  | Bariton
  | Bass
(** Not documented by MusicBrainz and therefore not exhaustive.
    We guess this by {i grepping} the [disambiguation] comment. *)

type instrument =
  | Piano
  | Violin
  | Viola
  | Guitar
(** Not documented by MusicBrainz and certainly not exhaustive.
    We guess this by {i grepping} the [disambiguation] comment. *)

type role =
  | Composer
  | Conductor
  | Singer of voice
  | Player of instrument
(** Not documented by MusicBrainz and therefore not exhaustive.
    We guess this by {i grepping} the [disambiguation] comment. *)

val role_to_string : role -> string

module Roles : Set.S with type elt = role

val no_role : Roles.t
val roles_of_string : string -> Roles.t
(** Scan the string for keywords. *)

type t =
  | Person of Roles.t
  | Group
  | Orchestra
  | Choir
  | Character
  | Other
  | Unknown of string
(** As documented by {{: https://musicbrainz.org/doc/Artist }MusicBrainz}.
    For us, only [Person], [Group], [Orchestra] and [Choir] should be relevant. *)

val compare : t -> t -> int
(** Compare in such a way that sorting a list of artists moves
    composers and conductors to the front, singers and instrumentalists
    next and ensembles, orchestras and choirs last.*)

val of_string : Roles.t -> string -> t
val to_string : t -> string

val is_composer : t -> bool
val is_conductor : t -> bool
