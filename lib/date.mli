(* date.mli -- part of PML (Physical Media Library)

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

(** A date with varying precision in format ["YYYY-MM-DD"]. *)

type t
(** {v
     <define name="def_incomplete-date">
         <data type="string">
             <param name="pattern">[0-9]{4}(-[0-9]{2})?(-[0-9]{2})?</param>
         </data>
     </define>
     v} *)

val of_year : ?month:int -> ?day:int -> int -> t

val to_string : t -> string
val year_to_string : t -> string
val of_opt_string_opt : string option -> t option

val compare : t -> t -> int

module Syntax : sig
  val ( = ) : t -> t -> bool
  val ( < ) : t -> t -> bool
  val ( <= ) : t -> t -> bool
  val ( > ) : t -> t -> bool
  val ( >= ) : t -> t -> bool
end
