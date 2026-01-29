module MBID_Set : Set.S with type elt = string

(** Note that we {e must not} replace [artist] elements by their MBID, since there can
    be additional elements, in particular [disambiguation].

    {e Is this really true?  Isn't [disambiguation] unique and can be gotten from
       looking up the [artist]?} *)

module Artist : sig

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

  val id : t -> MBID_Set.t
  (** Wrap the MBID in a set so that we can easily form sets without
      duplicates. *)

  val compare : t -> t -> int

  val update : t Cached.Artist.M.t -> t -> (t, string) result
  (** Find the artist with the same MBID in the dictionary.
      This is used to replace the artist record without
      lifespan included in releases by the artist record with
      liefspan.  *)

  val to_string : t -> string
  (** Exploration, debugging, etc. *)

end

module Artist_Credit : sig

  type t =
    { name : string option;
      artist : Artist.t option }
(** Essentially an indirection with the opportunity to give a
    shorter name for the artist.
    {v
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

  val artist_id : t -> MBID_Set.t
  (** Extract the artist's MBID inside the artist credit. *)

  val update_artist : Artist.t Cached.Artist.M.t -> t -> (t, string) result
  (** Do a [Artist.update] inside, if applicable. *)

  val to_string : t -> string
  (** Exploration, debugging, etc. *)

end

module Recording : sig

  type t =
    { id : string (** While this is optional in the DTD, it should be there anyway. *);
      title : string option;
      artist_credits : Artist_Credit.t list }
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

  val artist_ids : t -> MBID_Set.t
  (** Extract the MBID of all credited artists. *)

  val update_artists : Artist.t Cached.Artist.M.t -> t -> (t, string) result
  (** Do a [Artist.update] on all credited artists. *)

end

module Track : sig

  type t =
    { id : string (** While this is optional in the DTD, it should be there anyway. *);
      position : int option;
      title : string option;
      artist_credits : Artist_Credit.t list;
      recording : Recording.t option }
(** {v
     <define name="def_track-data">
         <optional>
             <attribute name="id">
                 <data type="anyURI"/>
             </attribute>
         </optional>
         <optional>
             <element name="position">
                 <data type="nonNegativeInteger"/>
             </element>
         </optional>
         <optional>
             <element name="number">
                 <text/>
             </element>
         </optional>
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
             <ref name="def_artist-credit"/>
         </optional>
         <optional>
             <ref name="def_recording-element"/>
         </optional>
     </define>
     v} *)

  val artist_ids : t -> MBID_Set.t
  (** Extract the MBID of all credited artists. *)

  val update_artists : Artist.t Cached.Artist.M.t -> t -> (t, string) result
  (** Do a [Artist.update] on all credited artists. *)

end

module Disc : sig

  type t =
    { id : string (** While this is optional in the DTD, it should be there anyway. *);
    }
(** {v
     <define name="def_disc-element">
         <element name="disc">
             <attribute name="id">
                 <data type="string">
                     <param name="pattern">[a-zA-Z0-9._]{27}-</param>
                 </data>
             </attribute>
             <ref name="def_disc-attribute_extension"/>
             <optional>
                 <element name="sectors">
                     <data type="nonNegativeInteger"/>
                 </element>
             </optional>
             <optional>
                 <ref name="def_offset-list"/>
             </optional>
             <optional>
                 <ref name="def_release-list"/>
             </optional>
             <ref name="def_disc-element_extension"/>
         </element>
     </define>
     v} *)

end

module Medium : sig

  type t =
    { id : string (** While this is optional in the DTD, it should be there anyway. *);
      position : int option;
      title : string option;
      discs : Disc.t list;
      tracks : Track.t list }
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

  val artist_ids : t -> MBID_Set.t
  (** Extract the MBID of all credited artists. *)

  val update_artists : Artist.t Cached.Artist.M.t -> t -> (t, string) result
  (** Do a [Artist.update] on all credited artists. *)

  val print : t -> unit

end

module Release : sig

  type t =
    { id : string (** While this is optional in the DTD, it should be there anyway. *);
      title : string option;
      artist_credits : Artist_Credit.t list;
      media : Medium.t list }
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

  val artist_ids : t -> MBID_Set.t
  (** Extract the MBID of all credited artists. *)

end

module Taggable : sig

  type t =
    { medium : Medium.t;
      release : Release.t;
      discid : string }

  val of_discid : ?medium:string -> root:string -> string -> (t, string) result
  (** Find the released disc matching the discid. *)

  val print : t -> unit
  (** Exploration, WIP ... *)

end

