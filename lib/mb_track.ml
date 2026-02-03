(* mb_track.ml -- part of PML (Physical Media Library)

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
  { id : string (** While this is optional in the DTD, it should be there anyway. *);
    position : int option;
    title : string option;
    artist_credits : Mb_artist_credit.t list;
    recording : Mb_recording.t option }

let make id position title artist_credits recording =
  let title = Edit.blank_to_none title in
  { id; position; title; artist_credits; recording }

let jsont =
  Jsont.Object.map ~kind:"Track" make
  |> Jsont.Object.mem "id" Jsont.string
  |> Jsont.Object.opt_mem "position" Jsont.int
  |> Jsont.Object.opt_mem "title" Jsont.string
  |> Jsont.Object.mem "artist-credit" Jsont.(list Mb_artist_credit.jsont) ~dec_absent:[]
  |> Jsont.Object.opt_mem "recording" Mb_recording.jsont
  |> Jsont.Object.finish

let artist_ids t =
  let artist_credits =
    List.map Mb_artist_credit.artist_id t.artist_credits |> Sets.mbid_union in
  match t.recording with
  | None -> artist_credits
  | Some recording -> Sets.MBID.union (Mb_recording.artist_ids recording) artist_credits

let update_artists map t =
  let open Result.Syntax in
  let* artist_credits =
    Result_list.map (Mb_artist_credit.update_artist map) t.artist_credits in
  match t.recording with
  | Some recording ->
     let* recording = Mb_recording.update_artists map recording in
     Ok { t with artist_credits; recording = Some recording }
  | None ->
     Ok { t with artist_credits }

let print n t =
  let open Printf in
  printf "  Trk%2d.%02d: %s\n"
    n
    (Option.value t.position ~default:0)
    (match t.title with
     | None | Some "" -> "[" ^ t.id ^ "]"
     | Some s -> s);
  begin match t.artist_credits with
  | [] -> ()
  | c :: clist ->
     printf "      Art.: %s\n" (Mb_artist_credit.to_string c);
     List.iter (fun c -> printf "            %s\n" (Mb_artist_credit.to_string c)) clist
  end;
  begin match t.recording with
  | None -> ()
  | Some r -> Mb_recording.print r
  end;
  ()
