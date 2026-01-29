(* lifespan.mli -- part of PML (Physical Media Library)

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

(** Birth and death of artists. *)

type t =
  | Alive of Date.t (** Known to be alive since. *)
  | Dead of Date.t * Date.t (** Known to be have lived from/to. *)
  | Dead' of Date.t (** Known to have lived until. *)
  | Limbo (** Nothing known. *)

(** We ignore the [ended] element.
    {v
     <element name="life-span">
         <optional>
             <element name="begin">
                 <ref name="def_incomplete-date"/>
             </element>
         </optional>
         <optional>
             <element name="end">
                 <ref name="def_incomplete-date"/>
             </element>
         </optional>
         <optional>
             <ref name="def_ended" />
         </optional>
     </element>
     v} *)

type relation = Before | After | Overlap
(** If intervals are disjoint, we can assign composer
    and performer roles unambigously. *)

val relation : t -> t -> relation
(** Check if intervals are disjoint. *)

val compare : t -> t -> int

val not_performer : ?cutoff:int -> t -> bool
(** If they have died before that year (the default is 1910),
    the artist can not be a performer and should be a composer. *) 

val jsont : t Jsont.t
val to_string : t -> string
