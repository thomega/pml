(* mb_raw.mli -- part of PML (Physical Media Library)

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

(** Cache and process JSON responses of the MusicBrainz database. *)

(** Process the JSON files returned by MusicBrainz uninterpreted.
    NB: except [normalize], all functions are for debugging only. *)

val normalize : string -> (string, string) result
(** Decode and encode the JSON in the string by [Jsont] to obtain
    a canonical representation. *)

val of_file : string -> (Jsont.json, string) result
(** Decode a JSON stored in a file. *)

val print_file : string -> unit
(** Decode a JSON stored in a file, encode it and write it to
    standard output. *)

val grep : rex:Pcre2.regexp -> string list -> string -> (unit, string) result
(** [grep ~rex path json] prints string values matching the compiled
    regular expression [rex] in the textual representation [json] of a
    JSON.  Prefix the matches by a dot-separated object name path
    rooted at [path]. *)

module MMap : Map.S with type key = string
module PSet : Set.S with type elt = string list

val matches : rex:Pcre2.regexp -> string list -> string -> (PSet.t MMap.t, string) result
(** [matches ~rex path json] collects all string values matching the compiled
    regular expression [rex] in the textual representation [json] of a JSON.
    They are returned as a map from the matching string to sets of dot-separated
    object name paths rooted at [path]. *)

val dump_schema_file : string -> unit
(** Decode a JSON stored in a file and write the structure to standard
    output. *)
