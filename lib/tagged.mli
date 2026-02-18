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

type multi =
  { tracks' : Track.t list; (** The tracks ripped as part of a group. This can consist
                                of a single track if the group contains tracks from
                                another disc.  *)
    width : int (** The width of the printed track number, including leading zeros. *)
  }
(** A sequence of tracks to be ripped as a group. *)

type tracks =
  | Single of Track.t (** The track is to be ripped as a single track. *)
  | Multi of multi (** The tracks are to be ripped as a sequence. *)
(** We treat single movement pieces specially. *)

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
    artists : Artist.Collection.t; (** The artists credited for the release. *)
    tracks : tracks; (** The tracks to be ripped, encoded and tagged. *)
    discid : string; (** The discid from which the audio was ripped. *)
    medium_title : string option;
    medium_id : string;
    release_title : string option;
    release_id : string
  }

val of_mb : Taggable.t -> t

type trackset =
  { offset : int; (** Number of earlier tracks stored on other CDs. *)
    first : int; (** First track to be included
                     (counting from 1, {e before} applying the offset). *)
    last : int option; (** Last track to be included
                           (counting from 1, {e before} applying the offset). *)
    width : int; (** The width of the printed track number, including leading zeros. *)
    single : bool (** Whether to rip the track as as single track and not as a part of
                      a multi track piece. *)
  }
(** Track subset selection. *)

val default_trackset : trackset

module Edits : sig

  type all =
    { trackset : trackset option (** Select tracks and apply offsets. *);
      recording_titles : bool (** Use the title the recording instead of the title of the track.
                                  There are MBID entries where the two differ slightly or
                                  are given in different languages. *);
      edit_track_titles : Perl.S.ranged list (** List of [perl]-style
                                                 [range/regexp/substitution/flags]
                                                 expressions for editing track titles. *);
      release_title : bool (** Pick the title of the whole release as the title of the selection. *);
      medium_title : bool (** Pick the title of the medium as the title of the selection. *);
      edit_prefix : Perl.S.t list (** Apply a [perl]-style [/regexp/substitution/flags]
                                      edit to the common prefix of the track titles,
                                      that we use as a candidate for the overall title. *);
      edit_title : Perl.S.t list (** Apply a [perl]-style [/regexp/substitution/flags]
                                     edit to the overall title. *);
      title : string option (** Set the title of the selection explicitely. *);
      delete_artists : Perl.M.ranged list (** List of [perl]-style [/regexp/flags] expressions
                                              for deleting artists with matching name. *);
      delete_artists_sort : Perl.M.ranged list (** List of [perl]-style [/regexp/flags] expressions
                                                   for deleting artists with matching sort-name. *);
      edit_artists : Perl.S.ranged list (** List of [perl]-style [range/regexp/substitution/flags]
                                            expressions for editing artist's names. *);
      add_artist : string Edit.ranged list;
      composer_pattern : Perl.M.t option (** Select the composer among the artists by a regexp. *);
      performer_pattern : Perl.M.t option (** Select the top-billed performer among the artists
                                              by a regexp. *);
      composer : string option (** Set the name of the composer explicitely. *);
      performer : string option (** Set the name of the top-billed performer explicitely. *);
      unitary : bool (** If the performer can't b separated from the composer, don't add the
                         name to the directory. *)
    }

  val apply_all : all -> t -> (t, string) result
  (** Apply all edits in the order of the fields of [all]. *)

end
(** Modify the default filenames and tags derived from MusicBrainz.
    The order is significant, of course.*)

val iter_tracks : (Track.t -> (unit, 'e) result) -> t -> (unit, 'e) result
(** Iterate over all tracks to be ripped. *)

val target_dir : t -> string * string
(** Where to write the encoded tracks.
    This belongs to [Rip], but that would introduce a cyclic dependence
    in the modules. *)

val print : ?no_artists:bool -> ?factor_artists:bool ->
            ?no_originals:bool -> ?no_recordings:bool -> t -> unit
(** Exploration, WIP ... *)
