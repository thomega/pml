(* mb_recording.mli -- part of PML (Physical Media Library)

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

(** Musicbrainz recording records. *)

type t =
  { id : string (** While this is optional in the DTD, it should be there anyway. *);
    title : string option;
    artist_credits : Mb_artist_credit.t list }

val jsont : t Jsont.t
(** JSON decoder. *)

val artist_ids : t -> Sets.MBID.t
(** Extract the MBID of all credited artists. *)

val update_artists : Mb_artist.t Cached.Artist.M.t -> t -> (t, string) result
(** Do a [Mb_artist.update] on all credited artists. *)

val print : t -> unit
(** Exploration, debugging, etc. *)

(** {v
     <define name="def_recording-element">
         <element name="recording">
             <optional>
                 <attribute name="id">
                     <data type="anyURI"/>
                 </attribute>
             </optional>
             <ref name="def_recording-attribute_extension"/>
             <optional>
                 <element name="title">
                     <text/>
                 </element>
             </optional>
             <optional>
                 <element name="length">
                     <data type="nonNegativeInteger"/>
                 </element>
             </optional>
             <optional>
               <ref name="def_annotation" />
             </optional>
             <optional>
                 <element name="disambiguation">
                     <text/>
                 </element>
             </optional>
             <optional>
                 <ref name="def_video"/>
             </optional>
             <optional>
                 <ref name="def_artist-credit"/>
             </optional>
             <optional>
                 <element name="first-release-date">
                     <ref name="def_incomplete-date" />
                 </element>
             </optional>
             <optional>
                 <ref name="def_release-list"/>
             </optional>
             <optional>
                 <ref name="def_alias-list"/>
             </optional>
             <optional>
                 <ref name="def_puid-list"/>
             </optional>
             <optional>
                 <ref name="def_isrc-list"/>
             </optional>
             <zeroOrMore>
                 <ref name="def_relation-list"/>
             </zeroOrMore>
             <optional>
                 <ref name="def_tag-list"/>
             </optional>
             <optional>
                 <ref name="def_user-tag-list"/>
             </optional>
             <optional>
                 <ref name="def_genre-list"/>
             </optional>
             <optional>
                 <ref name="def_user-genre-list"/>
             </optional>
             <optional>
                 <ref name="def_rating"/>
             </optional>
             <optional>
                 <ref name="def_user-rating"/>
             </optional>
             <ref name="def_recording-element_extension"/>
         </element>
     </define>
     v} *)
