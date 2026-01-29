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
