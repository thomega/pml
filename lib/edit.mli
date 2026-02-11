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

type ranges
(** All integers and finite subsets thereof.  It can be thought of an ['a option]
    type, where [None] represents all integers. *)

val in_range : int -> ranges -> bool
(** Set membership. *)

val ranges_to_list_opt : ranges -> int list option
(** All integers result in [None] and a (finite) subset produces
    [Some] ordered list of integers. *)

val ranges_to_string : ranges -> string
(** Used for error reporting.  All integers are represented by [""] and the
    empty subset by ["1-0"]. *)

type 'a ranged = ranges * 'a
(** Some expression qualified by a set of integers. *)

val ranged_of_string : (string -> ('a, string) result) -> string -> ('a ranged, string) result
(** [ranged_of_string parser "1,3-5,8expr"] produces something equivalent
    to [(Some [1,3,4,5,8], parser "expr")], where
    ["expr"] {e must not} start with a digit. *)
