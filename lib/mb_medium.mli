(* mb_medium.mli -- part of PML (Physical Media Library)

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

(** Musicbrainz medium records. *)

type t =
  { id : string; (** While this is optional in the DTD, it should be there anyway. *)
    position : int option;
    title : string option;
    discs : Mb_disc.t list;
    tracks : Mb_track.t list }

val jsont : t Jsont.t
(** JSON decoder. *)

val artist_ids : t -> Sets.MBID.t
(** Extract the MBID of all credited artists. *)

val update_artists : Mb_artist.t Cached.Artist.M.t -> t -> (t, string) result
(** Do a [Mb_artist.update] on all credited artists. *)

val print : t -> unit
(** Exploration, debugging, etc. *)

(** {v
     <define name="def_medium">
         <element name="medium">
             <optional>
                 <attribute name="id">
                     <ref name="def_uuid"/>
                 </attribute>
             </optional>
             <optional>
                 <element name="title">
                     <text/>
                 </element>
             </optional>
             <optional>
                 <element name="position">
                     <data type="nonNegativeInteger"/>
                 </element>
             </optional>
             <optional>
                 <element name="format">
                     <attribute name="id">
                         <ref name="def_uuid"/>
                     </attribute>
                     <text/>
                 </element>
             </optional>
             <optional>
                 <ref name="def_disc-list"/>
             </optional>
             <optional>
                 <ref name="def_pregap-track"/>
             </optional>
             <ref name="def_track-list"/>
             <optional>
                 <ref name="def_data-track-list"/>
             </optional>
         </element>
     </define>
     v} *)
