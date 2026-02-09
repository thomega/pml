(* release.mli -- part of PML (Physical Media Library)

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
