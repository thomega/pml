(** Musicbrainz track records. *)

type t =
  { id : string (** While this is optional in the DTD, it should be there anyway. *);
    position : int option;
    title : string option;
    artist_credits : Mb_artist_credit.t list;
    recording : Mb_recording.t option }

val jsont : t Jsont.t
(** JSON decoder. *)

val artist_ids : t -> Sets.MBID.t
(** Extract the MBID of all credited artists. *)

val update_artists : Mb_artist.t Cached.Artist.M.t -> t -> (t, string) result
(** Do a [Mb_artist.update] on all credited artists. *)

val print : int -> t -> unit
(** Exploration, debugging, etc. *)

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
