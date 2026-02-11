(* track.ml -- part of PML (Physical Media Library)

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
  { number : int;  (** Overall position of the track in the whole work, counting from 1. *)
    number_on_disc : int;
    title : string;
    recording_title : string option;
    artists : Artist.Collection.t;
    id : string }

let of_mb mb =
  let module T = Mb_track in
  let module R = Mb_recording in
  let id = mb.T.id
  and number = Option.value mb.T.position ~default:0 in
  let number_on_disc = number in

  let title, recording_title =
    match mb.T.title, mb.T.recording with
    | Some t, Some r ->
       begin match r.R.title with
       | Some rt ->
          if Ubase.from_utf8 t = Ubase.from_utf8 rt then
            (t, None)
          else
            (t, Some rt)
       | None -> (t, None)
       end
    | Some t, None -> (t, None)
    | None, Some r ->
       begin match r.R.title with
       | Some rt -> (rt, None)
       | None -> ("(untitled)", None)
       end
    | None, None -> ("(untitled)", None) in

  let artists =
    Artist.of_credits mb.T.artist_credits in
  let artists =
    match mb.T.recording with
    | Some r -> Artist.Collection.union (Artist.of_credits r.R.artist_credits) artists
    | None -> artists in

  { id; number; number_on_disc; title; recording_title; artists }

let recording_title t =
  match t.recording_title with
  | Some title -> { t with title; recording_title = None }
  | None -> t

let filter_artists predicate t =
  { t with artists = Artist.Collection.filter predicate t.artists }

let map_artists f t =
  let open Result.Syntax in
  let* artists = Artist.map_result f t.artists in
  Ok { t with artists }
