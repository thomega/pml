(* mb_medium.ml -- part of PML (Physical Media Library)

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

type t =
  { id : string; (** While this is optional in the DTD, it should be there anyway. *)
    position : int option;
    title : string option;
    discs : Mb_disc.t list;
    tracks : Mb_track.t list }

let make id position title discs tracks =
  let discs = Option.value ~default:[] discs
  and tracks = Option.value ~default:[] tracks
  and title = Edit.blank_to_none title in
  { id; position; title; discs; tracks }

let jsont =
  Jsont.Object.map ~kind:"Medium" make
  |> Jsont.Object.mem "id" Jsont.string
  |> Jsont.Object.opt_mem "position" Jsont.int
  |> Jsont.Object.opt_mem "title" Jsont.string
  |> Jsont.Object.opt_mem "discs" Jsont.(list Mb_disc.jsont)
  |> Jsont.Object.opt_mem "tracks" Jsont.(list Mb_track.jsont)
  |> Jsont.Object.finish

let artist_ids m =
  List.map Mb_track.artist_ids m.tracks |> Sets.mbid_union

let update_artists map m =
  let open Result.Syntax in
  let* tracks = Result_list.map (Mb_track.update_artists map) m.tracks in
  Ok { m with tracks }

let print m =
  let open Printf in
  let n = Option.value m.position ~default:0 in
  printf "Disc %2d: %s\n"
    n
    (match m.title with
     | None | Some "" -> "[" ^ m.id ^ "]"
     | Some s -> s);
  List.iter (Mb_track.print n) m.tracks;
  ()
