(* taggable.mli -- part of PML (Physical Media Library)

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

(** MusicBrainz records for a medium together with its release.*)

(** The data structures in [Mb_release] are a slightly embellished translation
    of the JSON response for the [release] containing a particular disc.
    Currently, the only embellishment is the [Lifespan.t] of the artists.

    Therefore, we have a hierarchy
    {v
     release
      - title
      - artist_credit*
         - name
         - artist
      - medium*
         - position
         - title
         - track*
            - position
            - title
            - artist_credit*
               - name
               - artist
            - recording?
               - title
               - artist_credit*
                  - name
                  - artist
     v}
    and we have to take care of two things for tagging the rip of a disk
    {ul {- partially invert the hierarchy to put a reference to the release
           into the medium.}
        {- disambiguate the titles and artists of release, medium, track
           and recording.}}
 *)

type t =
  { medium : Mb_medium.t;
    release : Mb_release.t;
    discid : string }

val of_discid_sans_lifespans : ?medium:string -> root:string -> string -> (t, string) result
val of_discid : ?medium:string -> root:string -> string -> (t, string) result
(** Find the released disc matching the discid.
    Behind the scenes, it also inserts the extended [artist] records
    from the cache and updates the latter from MusicBrainz, if necessary. *)

val print : t -> unit
(** Exploration, WIP ... *)
