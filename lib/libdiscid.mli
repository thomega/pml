(* libdiscid.mli -- part of PML (Physical Media Library)

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

(** Compute the disc identifier(s) used by thr MusicBrainz database. *)

type t = private
  { id : string; (** The disc identifier used by MusicBrainz.
                     It is a 28 character BASE64 string
                     made URL safe by replacing ['+'], ['/']
                     and the padding character ['='] with
                     ['.'], ['_'] and ['-'] respectively. *)
    freedb : string (** The disc identifier use by FreeDB. (obsolete) *);
    toc : string; (** The TOC of the disc, used for fuzzy searches.
                     The spaces are already replaced by ['+']. *)
    submission_url : string (** URL for submitting a diskid to MusicBrainz. *)
  }
(** See the {{: https://musicbrainz.org/doc/Disc_ID_Calculation }MusicBrainz documentation}
    for details. *)

val get : ?device:string -> unit -> (t, string) result
(** Get the disc identifiers from the CD in [device].
    The default for [device] is taken from [default_device ()]. *)

val default_device : unit -> string
(** Usually ["/dev/cdrom"]. *)
