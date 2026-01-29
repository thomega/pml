(* mb_error.mli -- part of PML (Physical Media Library)

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

(** JSON error response from Musicbrainz. *)

(** Use [Error.get_error_opt] to test if the response contains
    an [error] element with a message. *)

type t = private { error : string; help : string option }
(** On Error, Musicbrainz returns and error message *)

val get_error_opt : string -> string option
(** Check if the JSON contains a top level [error] element.
    Ignores parsing errors.  They must be handled subsequently. *)

