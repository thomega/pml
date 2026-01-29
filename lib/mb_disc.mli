(** Musicbrainz disc records. *)

type t =
  { id : string (** While this is optional in the DTD, it should be there anyway. *);
  }
(** We only need the discid. *)

val jsont : t Jsont.t
(** JSON decoder. *)

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
