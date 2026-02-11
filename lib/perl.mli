(* perl.mli -- part of PML (Physical Media Library)

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

(** Use the PCRE library to implement [perl]-style string matching and substitutions.*)

module M : sig

  type t
  (** Compiled regular expression. *)

  val of_string : string -> (t, string) result
  (** Parse and compile a [perl] style [/regexp/flags/] matching.
      We separate parsing and compilation from the application to be able to
      handle errors early.  The slight performance benefit for repeated applications
      is not very important in our application. *)

  val to_string : t -> string
  (** For error messages. *)

  val exec : t -> string -> bool
  (** Attempt a [perl] style [/regexp/flags/] matching. *)

  val exec' : string -> string -> (bool, string) result
  (** Combining [of_string] and [exec] for convenience. *)

  type ranged = t Edit.ranged
  val ranged_of_string : string -> (ranged, string) result
  (** [ranges/regexp/flags], where [ranges] has, e.g. the form [1,3-5]
      and will apply items 1, 3, 4, and 5 only. *)

  val ranged_to_string : ranged -> string

end
(** Matchings. *)

module S : sig

  type t
  (** Compiled regular expression and substitution template. *)

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
  (** Combining [of_string] and [exec] for convenience. *)

  type ranged = t Edit.ranged
  val ranged_of_string : string -> (ranged, string) result
  (** [ranges/regexp/substitution/flags], where [ranges] has, e.g. the form [1,3-5]
      and will apply items 1, 3, 4, and 5 only. *)

  val ranged_to_string : ranged -> string

end
(** Substitutions.*)
