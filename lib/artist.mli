(* artist.mli -- part of PML (Physical Media Library)

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

(** All we know about an artist. *)

type t =
  { name : string; (** Used in tags. *)
    sort_name : string; (** Used in directory names. *)
    artist_type : Artist_type.t; (** Determines the credit order. *)
    lifespan : Lifespan.t; (** Can separate composers from performers. *)
    id : string (** MBID *) }

val compare : t -> t -> int
(** Compare artists by [artist_type] and use [lifespan] and [name] as a tie breakers. *)

val of_mb : Mb_artist.t -> t
(** Translate and replace [None] by defaults.  *)

val of_name : string -> t
(** Make up a name, without further information. *)

val of_sort_name : string -> t
(** Make up a name, without further information. *)

val of_name_sort_name : string -> string -> t
(** Make up a name, without further information. *)

module Collection : Set.S with type elt = t

val map_result : (t -> (t, 'e) result) -> Collection.t -> (Collection.t, 'e) result
(** Like [Collection.map], but with error exits. *)

val lifespan_gaps : Collection.t -> Collection.t list
(** Check if there are artists, who died before others where born.
    Such artists must be the composer(s). *)

val of_credits : Mb_artist_credit.t list -> Collection.t
(** Follow MusicBrainz' indirections. *)

val to_string : ?sortable:bool -> t -> string
(** Format [name], [artist_type], and [lifespan]. *)
