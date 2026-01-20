(** Birth and death of artists. *)

type t =
  | Alive of Date.t (** Known to be alive since. *)
  | Dead of Date.t * Date.t (** Known to be have lived from/to. *)
  | Dead' of Date.t (** Known to have lived until. *)
  | Limbo (** Nothing known. *)

(** We ignore the [ended] element.
    {v
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
     v} *)

type relation = Before | After | Overlap
(** If intervals are disjoint, we can assign composer
    and performer roles unambigously. *)

val relation : t -> t -> relation
(** Check if intervals are disjoint. *)

val compare : t -> t -> int

val not_performer : ?cutoff:int -> t -> bool
(** If they have died before that year (the default is 1910),
    the artist can not be a performer and should be a composer. *) 

val jsont : t Jsont.t
val to_string : t -> string
