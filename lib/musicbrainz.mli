(** Cache and process JSON responses of the MusicBrainz database. *)

module type Error =
  sig

    type t = { error : string; help : string option }
    (** On Error, Musicbrainz returns and error message *)

    val get_error_opt : string -> string option
    (** Check if the JSON contains a top level [error] element.
        Ignores parsing errors.  They must be handled subsequently. *)

  end
(** JSON error response from Musicbrainz. *)

module Error : Error
(** Use [Error.get_error_opt] to test if the response contains
    an [error] element with a message. *)

module type Raw =
  sig

    val normalize : string -> (string, string) result
    (** Decode and encode the JSON in the string by [Jsont] to obtain
        a canonical representation. *)

    val of_file : string -> (Jsont.json, string) result
    (** Decode a JSON stored in a file. *)

    val print_file : string -> unit
    (** Decode a JSON stored in a file, encode it and write it to
        standard output. *)

    val dump_schema_file : string -> unit
    (** Decode a JSON stored in a file and write the structure to
        standard output. *)

  end
(** Process the JSON files returned by MusicBrainz uninterpreted.
    NB: except [normalize], all functions are for debugging only. *)

module Raw : Raw

module type Cached =
  sig

    val get : root:string -> string -> (string, string) result
    (** Return the JSON for the given key, preferring the cache located at [root].
        If there is no local entry for the key, it will be created from the
        result of the remote lookup. *)

    val local : root:string -> string -> (string option, string) result
    (** Return the JSON for the given key, using only the cache located at [root]. *)

    val remote : string -> (string, string) result
    (** Return the JSON for the given key, ignoring any cache. *)

    val all_local : root:string -> ((string * string) list, string) result
    (** Return the cached [key] JSON pairs. *)

    val url : string -> (string, string) result
    (** Return the URL for querying Musicbrainz for the entry corresponding to a key. *)

    module Internal : Cache.T with type key = string and type value = string
    (** Access the public interface of the [Cache.T] used to implement
        this cached table. *)

  end
(** Query the Musicbrainz database with local caching to avoid excessive network
    traffic when finetuning the tagging of tracks. *)

module Discid_cached : Cached
(** Access information about a disc, in particular the MBIDs of the
    releases containing the disc, by the [discid]. *)

(** {v
     <element name="disc">
         <attribute name="id">
             <data type="string">
                 <param name="pattern">[a-zA-Z0-9._]{27}-</param>
             </data>
         </attribute>
         ...    
     </element>
     v} *)

(** The
    {{: https://github.com/metabrainz/libmusicbrainz/blob/master/examples/cdlookup_c.c }old example}
    in the MusicBrainz sources suggests that it is impossible to
    get all information from a single [discid] lookup.  This is (no longer?)
    correct: we could access all required information in one step by
    adding [inc=artist-credits+recordings] to the request.   However,
    this results in substational redundancy, whenever a release contains
    many discs, as the information is repeated for every disc in the release.

    Therefore, we continue to use the two step approach suggested by the example. *)

val releases_of_discid : root:string -> string -> (string list, string) result
(** There can be more than one release of a given disc.  Return
    them as a list of MBID strings. *)

val valid_mbid : string -> (string, string) result
(** Check that a string is a MBID (i.e. UUID)
    {v
     <define name="def_uuid">
         <data type="string">
             <param name="pattern">[0-9a-f]{8}(-[0-9a-f]{4}){3}-[0-9a-f]{12}</param>
         </data>
     </define>
     v}
     Note that this does not reject upper case hexadecimals and
     silently changes them to lowercase instead. *)

module Release_cached : Cached
(** Access more detailled information about a release from its MBID: tracks, artists, etc. *)

module Artist_cached : Cached
(** Access more detailled information about an artist: life span and aliases.
    {b The lifespan can be used for distinguishing composers from performers!} *)

module Date : sig

  type t

  val of_opt_string_opt : string option -> t option

  val compare : t -> t -> int

  module Syntax : sig
    val ( = ) : t -> t -> bool
    val ( < ) : t -> t -> bool
    val ( <= ) : t -> t -> bool
    val ( > ) : t -> t -> bool
    val ( >= ) : t -> t -> bool
  end

end
(** A date with varying precision in format ["YYYY-MM-DD"]. *)

(** {v
     <define name="def_incomplete-date">
         <data type="string">
             <param name="pattern">[0-9]{4}(-[0-9]{2})?(-[0-9]{2})?</param>
         </data>
     </define>
     v} *)

module Lifespan : sig

  type t

  type relation =
    | Before
    | After
    | Overlap
  (** If intervals are disjoint, we can assign composer
      and performer roles unambigously. *)

  val relation : t -> t -> relation
  (** Check if intervals are disjoint. *)

end

module SSet : Set.S with type elt = string

(** Note that we {e must not} replace [artist] elements by their MBID, since there can
    be additional elements, in particular [disambiguation].

    {e Is this really true?  Isn't [disambiguation] unique and can be gotten from
       looking up the [artist]?} *)

module Artist : sig

  type artist_type =
    | Person
    | Group
    | Orchestra
    | Choir
    | Character
    | Other
    | Extended of string

  val artist_type_of_string : string -> artist_type
  val artist_type_to_string : artist_type -> string

  type t =
    { id : string; (** The MBID (i.e. UUID) of the artist.  While this is
                       declared as optional in the DTD, we can safely assume
                       that is always there. *)
      name : string option; (** The common name of the artist. *)
      sort_name : string option; (** The name in last name, first name order
                                     for persons and with articles removed
                                     from ensemble names. *)
      artist_type : artist_type option;
      lifespan : Lifespan.t option;
      disambiguation : string option; (** {e Is there an exhaustive list?} *)
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

  val id : t -> string
  val id' : t -> SSet.t

  val to_string : t -> string

end

module Artist_Credit : sig

  type t =
    { name : string option;
      artist : Artist.t option }
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

  val artists : t -> string list
  val artists' : t -> SSet.t

  val to_string : t -> string

end

module Recording : sig

  type t =
    { id : string (** While this is optional in the DTD, it should be there anyway. *);
      title : string option;
      artist_credit : Artist_Credit.t list }
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

  val artists : t -> string list
  val artists' : t -> SSet.t

end

module Track : sig

  type t =
    { id : string (** While this is optional in the DTD, it should be there anyway. *);
      position : int option;
      title : string option;
      artist_credit : Artist_Credit.t list;
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

  val artists : t -> string list
  val artists' : t -> SSet.t

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
      discs : Disc.t list; (** Is this relevant for us?  There are no titles or credits. *)
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

  val artists : t -> string list
  val artists' : t -> SSet.t

  val print : t -> unit

end

module Release : sig

  type t =
    { id : string (** While this is optional in the DTD, it should be there anyway. *);
      title : string option;
      artist_credit : Artist_Credit.t list;
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

end

type disc =
  { medium : Medium.t;
    title : string option;
    artist_credit : Artist_Credit.t list }

val disc_of_discid : root:string -> string -> (disc, string) result
(** Find the released disc matching the discid. *)

val artists_on_disc : disc -> string list

val print_disc : disc -> unit
(** Exploration, WIP ... *)

