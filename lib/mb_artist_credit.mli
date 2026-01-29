(* mb_artist_credit.mli -- part of PML (Physical Media Library)

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

(** (Named) pointer to artist. *)

type t =
  { name : string option;
    artist : Mb_artist.t option }
(** Essentially an indirection with the opportunity to give a
    shorter name for the artist. *)

val jsont : t Jsont.t
(** JSON decoder. *)

val artist_id : t -> Sets.MBID.t
(** Extract the artist's MBID inside the artist credit. *)

val update_artist : Mb_artist.t Cached.Artist.M.t -> t -> (t, string) result
(** Do a [Mb_artist.update] inside, if applicable. *)

val to_string : t -> string
(** Exploration, debugging, etc. *)

(** {v
     <define name="def_artist-credit">
         <element name="artist-credit">
             <optional>
                 <attribute name="id">
                     <ref name="def_uuid"/>
                 </attribute>
             </optional>
             <oneOrMore>
                 <element name="name-credit">
                     <optional>
                         <attribute name="joinphrase">
                             <text/>
                         </attribute>
                     </optional>
                     <optional>
                         <element name="name">
                             <text/>
                         </element>
                     </optional>
                     <ref name="def_artist-element"/>
                 </element>
             </oneOrMore>
         </element>
     </define>
     v} *)

