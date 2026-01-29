(* rip.ml -- part of PML (Physical Media Library)

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

(** We use a prefix for the WAV file, because discids can start with a period. *)
let wav_prefix = "cd-"

let script d =
  let open Printf in
  let wav_name i = sprintf "%s%s%02d.wav" wav_prefix d.Tagged.discid i in
  let separator () =
    printf "########################################################################\n" in
  printf "#! /bin/sh\n";
  separator ();
  printf "DISCID='%s'\n" d.Tagged.discid;
  printf "MEDIUM='%s'\n" d.Tagged.medium_id;
  printf "RELEASE='%s'\n" d.Tagged.release_id;
  separator ();
  printf "\n";
  separator ();
  printf "# Rip CD track unless the output file exists\n";
  separator ();
  printf "\n";
  printf "rip_track () {\n";
  printf "  if [ ! -r $2 ]; then\n";
  printf "    cdparanoia -w $1 $2\n";
  printf "  fi\n";
  printf "}\n";
  printf "\n";
  List.iter
    (fun t ->
      let n = t.Track.number_on_disc in
      printf "rip_track %2d %s\n" n (wav_name n))
    d.Tagged.tracks;
  printf "\n";
  separator ();
  printf "# Set up target directory\n";
  separator ();
  printf "\n";
  let root =
    match d.composer with
    | Some c -> c.Artist.name
    | None -> "Anonymous" in
  printf "ROOT=\"%s\"\n" root;
  let subdir =
    match d.Tagged.titles, d.Tagged.performer with
    | [], None -> "Unnamed"
    | t :: _, None -> Tagged.title_to_string t
    | [], Some p -> p.Artist.name
    | t :: _, Some p -> sprintf "%s - %s" (Tagged.title_to_string t) p.Artist.name in
  printf "SUBDIR=\"%s\"\n" subdir;
  printf "DIR=\"$ROOT/$SUBDIR\"\n";
  printf "mkdir -p \"$DIR\"\n";
  printf "\n";
  separator ();
  printf "# Encode and tag\n";
  separator ();
  printf "\n";
  List.iter
    (fun t ->
      printf "WAV=%s\n" (wav_name t.Track.number_on_disc);
      printf "TITLE=\"%0*d %s\"\n" d.track_width t.Track.number t.Track.title;
      printf "\n")
    d.tracks;
  Ok ()

