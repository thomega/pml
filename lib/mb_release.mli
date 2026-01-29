(* mb_release.mli -- part of PML (Physical Media Library)

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

(** Musicbrainz release records. *)

type t =
  { id : string (** While this is optional in the DTD, it should be there anyway. *);
    title : string option;
    artist_credits : Mb_artist_credit.t list;
    media : Mb_medium.t list }

val jsont : t Jsont.t
(** JSON decoder. *)

val artist_ids : t -> Sets.MBID.t
(** Extract the MBID of all credited artists. *)

val update_artists : Mb_artist.t Cached.Artist.M.t -> t -> (t, string) result
(** Do a [Mb_artist.update] on all credited artists. *)

(** {v
     <define name="def_release-element">
         <element name="release">
             <optional>
                 <attribute name="id">
                     <data type="anyURI"/>
                 </attribute>
             </optional>
             <ref name="def_release-attribute_extension"/>
             <optional>
                 <element name="title">
                     <text/>
                 </element>
             </optional>
             <optional>
                 <element name="status">
                     <attribute name="id">
                         <ref name="def_uuid"/>
                     </attribute>
                     <text/>
                 </element>
             </optional>
             <optional>
                 <element name="quality">
                     <ref name="def_quality"/>
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
                 <element name="packaging">
                     <attribute name="id">
                         <ref name="def_uuid"/>
                     </attribute>
                     <text/>
                 </element>
             </optional>
             <optional>
                 <element name="text-representation">
                     <optional>
                         <element name="language">
                             <ref name="def_iso-639"/>
                         </element>
                     </optional>
                     <optional>
                         <element name="script">
                             <ref name="def_iso-15924"/>
                         </element>
                     </optional>
                 </element>
             </optional>
             <optional>
                 <ref name="def_artist-credit"/>
             </optional>
             <optional>
                 <ref name="def_alias-list"/>
             </optional>
             <optional>
                 <ref name="def_release-group-element"/>
             </optional>
             <optional>
                 <element name="date">
                     <ref name="def_incomplete-date"/>
                 </element>
             </optional>
             <optional>
                 <element name="country">
                     <ref name="def_iso-3166-1-code"/>
                 </element>
             </optional>
             <optional>
                 <ref name="def_release-event-list"/>
             </optional>
             <optional>
                 <element name="barcode">
                     <text/>
                 </element>
             </optional>
             <optional>
                 <element name="asin">
                     <data type="string">
                         <param name="pattern">[A-Z0-9]{10}</param>
                     </data>
                 </element>
             </optional>
             <optional>
                 <ref name="def_cover-art-archive"/>
             </optional>
             <optional>
                 <ref name="def_label-info-list"/>
             </optional>
             <optional>
                 <ref name="def_medium-list"/>
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
                 <ref name="def_collection-list"/>
             </optional>
             <ref name="def_release-element_extension"/>
         </element>
     </define>
     v} *)
