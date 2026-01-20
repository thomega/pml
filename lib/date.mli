(** A date with varying precision in format ["YYYY-MM-DD"]. *)

type t
(** {v
     <define name="def_incomplete-date">
         <data type="string">
             <param name="pattern">[0-9]{4}(-[0-9]{2})?(-[0-9]{2})?</param>
         </data>
     </define>
     v} *)

val to_string : t -> string
val year_to_string : t -> string
val of_opt_string_opt : string option -> t option

val compare : t -> t -> int

module Syntax : sig
  val ( = ) : t -> t -> bool
  val ( < ) : t -> t -> bool
  val ( <= ) : t -> t -> bool
  val ( > ) : t -> t -> bool
  val ( >= ) : t -> t -> bool
end
