(* tagged.mli -- part of PML (Physical Media Library)

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

(** Tags and file system layout for musical works ripped from disc(s). *)

type title =
  | User of string (** User selected. *)
  | Tracks of string (** Longest common prefix of the tracks *)
  | Medium of string (** Disc. *)
  | Release of string (** Release. *)
(** The origin of the title. *)

val title_to_string : title -> string
(** Just the title, ignoring the origin. *)

type trackset =
  { offset : int; (** Number of earlier tracks stored on other CDs. *)
    first : int; (** First track to be included
                     (counting from 1, {e before} applying the offset). *)
    last : int option; (** Last track to be included
                           (counting from 1, {e before} applying the offset). *)
    width : int (** The width of the printed track number, including leading zeros. *)
  }
(** Track subset selection. *)

val default_trackset : trackset

type t =
  { composer : Artist.t option; (** The primary sorting key for the ripped files.
                                    From this, we will derive the name of the top
                                    level directory for storing the files.
                                    For classical music, this will be the composer.
                                    For popular music, it will be the top billed
                                    performer. *) 
    titles : title list; (** Possible titles of the work.
                             From this and the [performer], if present,
                             we will derive the name of the second level
                             directory for storing the files. *)
    performer : Artist.t option; (** The top billed performer for classical music, 
                                     to distinguish different interpretations.
                                     Empty for popular music. *)
    artists : Artist.Collection.t;
    tracks : Track.t list;
    tracks_orig : Track.t list option; (** The tracks with the original names iff a common
                                           prefix has been stripped to be used as title. *)
    track_width : int; (** The width of the printed track number, including leading zeros. *)
    discid : string; (** The discid from which the audio was ripped. *)
    medium_title : string option;
    medium_id : string;
    release_title : string option;
    release_id : string
  }

val of_mb : Taggable.t -> t

module Edits : sig

  type all =
    { title : string option;
      edit_prefix : Edit.perl_s option;
      edit_title : Edit.perl_s option;
      recording_titles : bool;
      medium_title : bool;
      release_title : bool;
      composer : string option;
      composer_prefix : string option;
      performer : string option;
      performer_prefix : string option;
      trackset : trackset option }

  val apply_all : all -> t -> (t, string) result

end

val target_dir : t -> string * string
(** Where to write the encoded tracks.
    This belongs to [Rip], but that would introduce a cyclic dependence
    in the modules. *)

val print : ?no_artists:bool -> ?factor_artists:bool ->
            ?no_originals:bool -> ?no_recordings:bool -> t -> unit
(** Exploration, WIP ... *)
