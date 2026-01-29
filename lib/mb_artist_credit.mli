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

