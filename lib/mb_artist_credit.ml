(* mb_artist_credit.ml -- part of PML (Physical Media Library)

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
  { name : string option;
    artist : Mb_artist.t option }

let make name artist =
  let name = Edit.blank_to_none name in
  { name; artist }

let jsont =
  Jsont.Object.map ~kind:"Artist_Credit" make
  |> Jsont.Object.opt_mem "name" Jsont.string
  |> Jsont.Object.opt_mem "artist" Mb_artist.jsont
  |> Jsont.Object.finish

let artist_id c =
  match c.artist with
  | None -> Sets.MBID.empty
  | Some artist -> Mb_artist.id artist
      
let update_artist map c =
  let open Result.Syntax in
  match c.artist with
  | Some artist ->
     let* artist = Mb_artist.update map artist in
     Ok { c with artist = Some artist }
  | None -> Ok c

let to_string c =
  match c.artist with
  | Some artist -> Mb_artist.to_string artist
  | None -> Option.value c.name ~default:"(anonymous)"
