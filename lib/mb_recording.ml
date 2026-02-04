(* mb_recording.ml -- part of PML (Physical Media Library)

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
    title : string option;
    artist_credits : Mb_artist_credit.t list }

let make id title artist_credits =
  let title = Edit.blank_to_none title in
  { id; title; artist_credits }

let jsont =
  Jsont.Object.map ~kind:"Recording" make
  |> Jsont.Object.mem "id" Jsont.string
  |> Jsont.Object.opt_mem "title" Jsont.string
  |> Jsont.Object.mem "artist-credit" Jsont.(list Mb_artist_credit.jsont) ~dec_absent:[]
  |> Jsont.Object.finish

let artist_ids r =
  List.map Mb_artist_credit.artist_id r.artist_credits |> Sets.mbid_union

let update_artists map r =
  let open Result.Syntax in
  let* artist_credits =
    Result_list.map (Mb_artist_credit.update_artist map) r.artist_credits in
  Ok { r with artist_credits }

let print r =
  let open Printf in
  printf "   Recording: %s\n"
    (match r.title with
     | None | Some "" -> "[" ^ r.id ^ "]"
     | Some s -> s);
  begin match r.artist_credits with
  | [] -> ()
  | c :: clist ->
     printf "Rec.-Artists: %s\n" (Mb_artist_credit.to_string c);
     List.iter (fun c -> printf "              %s\n" (Mb_artist_credit.to_string c)) clist
  end;
  ()
