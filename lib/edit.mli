(* edit.mli -- part of PML (Physical Media Library)

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

(** Edit strings. *)

val common_prefix : string list -> string * string list
(** Find the longest common prefix in a list of string
    and return it together with the tails. *)

val blank_to_none : string option -> string option
(** Map blank strings to [None]. *)

val filename_safe : string -> string
(** Sanitize filename. *)

val shell_single_quote : string -> string
val shell_double_quote : string -> string

module Perl_s : sig

  type t

  val of_string : string -> (t, string) result
  (** Parse and compile a [perl] style [/regexp/substitution/flags/] substitution.
      We separate parsing and compilation from the application to be able to
      handle errors early.  The slight performance benefit for repeated applications
      is not very important in our application. *)

  val to_string : t -> string
  (** For error messages. *)

  val exec : t -> string -> (string, string) result
  (** Apply a [perl] style [/regexp/substitution/flags/] substitution. *)

  val exec' : string -> string -> (string, string) result
  (** Combining [perl_s_of_string] and [perl_s]. *)

end
