(* track.mli -- part of PML (Physical Media Library)

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

(** All we know about a track/recording. *)

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
