(* mb_artist.mli -- part of PML (Physical Media Library)

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

(** Musicbrainz artist records. *)

(** Note that we {e must not} replace [artist] elements by their MBID, since there can
    be additional elements, in particular [disambiguation].

    {e Is this really true?  Isn't [disambiguation] unique and can be gotten from
       looking up the [artist]?} *)

type t =
  { id : string; (** The MBID (i.e. UUID) of the artist.  While this is
                     declared as optional in the DTD, we can safely assume
                     that is always there. *)
    name : string option; (** The common name of the artist. *)
    sort_name : string option; (** The name in last name, first name order
                                   for persons and with articles removed
                                   from ensemble names. *)
    artist_type : Artist_type.t option;
    lifespan : Lifespan.t option;
    disambiguation : string option; (** Keeping it around, but [artist_type] should suffice. *)
  }

val jsont : t Jsont.t
(** JSON decoder. *)

val id : t -> Sets.MBID.t
(** Wrap the MBID in a set so that we can easily form sets without duplicates. *)

val compare : t -> t -> int
(** Ordering according to [Artist_type.t]. *)

val update : t Cached.Artist.M.t -> t -> (t, string) result
(** Find the artist with the same MBID in the dictionary.
    This is used to replace the artist record without
    lifespan included in releases by the artist record with
    liefspan.  *)

val to_string : t -> string
(** Exploration, debugging, etc. *)

(** {v
     <define name="def_artist-element">
         <element name="artist">
             <optional>
                 <attribute name="id">
                     <data type="anyURI"/>
                 </attribute>
             </optional>
             <optional>
                 <attribute name="type">
                     <data type="anyURI"/>
                 </attribute>
             </optional>
             <optional>
                 <attribute name="type-id">
                     <ref name="def_uuid"/>
                 </attribute>
             </optional>
             <ref name="def_artist-attribute_extension"/>
             <optional>
                 <element name="name">
                     <text/>
                 </element>
             </optional>
             <optional>
                 <element name="sort-name">
                     <text/>
                 </element>
             </optional>
             <optional>
                 <element name="gender">
                     <attribute name="id">
                         <ref name="def_uuid"/>
                     </attribute>
                     <text/>
                 </element>
             </optional>
             <optional>
                 <element name="country">
                     <ref name="def_iso-3166-1-code"/>
                 </element>
             </optional>
             <optional>
                 <ref name="def_area-element"/>
             </optional>
             <optional>
                 <element name="begin-area">
                   <ref name="def_area-element_inner"/>
                 </element>
             </optional>
             <optional>
                 <element name="end-area">
                   <ref name="def_area-element_inner"/>
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
                 <element name="ipi">
                     <ref name="def_ipi"/>
                 </element>
             </optional>
             <optional>
               <ref name="def_ipi-list" />
             </optional>
             <optional>
               <ref name="def_isni-list" />
             </optional>
             <optional>
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
             </optional>
             <optional>
                 <ref name="def_alias-list"/>
             </optional>
             <optional>
                 <ref name="def_recording-list"/>
             </optional>
             <optional>
                 <ref name="def_release-list"/>
             </optional>
             <optional>
                 <ref name="def_release-group-list"/>
             </optional>
             <optional>
                 <ref name="def_work-list"/>
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
             <ref name="def_artist-element_extension"/>
         </element>
     </define>
     v} *)

