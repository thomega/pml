module Release : sig

  type t =
    { id : string (** While this is optional in the DTD, it should be there anyway. *);
      title : string option;
      artist_credits : Mb_artist_credit.t list;
      media : Mb_medium.t list }
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

  val artist_ids : t -> Sets.MBID.t
  (** Extract the MBID of all credited artists. *)

end

module Taggable : sig

  type t =
    { medium : Mb_medium.t;
      release : Release.t;
      discid : string }

  val of_discid : ?medium:string -> root:string -> string -> (t, string) result
  (** Find the released disc matching the discid. *)

  val print : t -> unit
  (** Exploration, WIP ... *)

end

