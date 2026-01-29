(* query.mli -- part of PML (Physical Media Library)

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

(** Query the MusicBrainz API. *)

type api
(** Options defining a web API. *)

val musicbrainz : api
(** The web API for lookups in the MusicBrainz data base. *)

type query =
  { table : string;
    inc : string list }
(** The MusicBrainz table to be queried and the additional
    information to be included in the result.*)

val exec : api -> query -> string -> (string, string) result
(** Perform a lookup for a key in the MusicBrainz database table
    specified by the [query].  If successful, the result is a
    JSON string. *)

val url : api -> query -> string -> string
(** Return the URL corresponding to the above query.
    This can be pasted into a web browser for debugging. *)

val curl : ?timeout:int -> user_agent:string -> string -> (string, string) result
(** Query a url directly. For debugging. *)




