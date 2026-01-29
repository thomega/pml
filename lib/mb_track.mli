(* mb_track.mli -- part of PML (Physical Media Library)

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
