(* mb_release.ml -- part of PML (Physical Media Library)

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
    artist_credits : Mb_artist_credit.t list;
    media : Mb_medium.t list }

let make id title artist_credits media =
  let title = Edit.blank_to_none title
  and artist_credits = Option.value ~default:[] artist_credits
  and media = Option.value ~default:[] media in
  { id; title; artist_credits; media }

let jsont =
  Jsont.Object.map ~kind:"Release" make
  |> Jsont.Object.mem "id" Jsont.string
  |> Jsont.Object.opt_mem "title" Jsont.string
  |> Jsont.Object.opt_mem "artist-credit" Jsont.(list Mb_artist_credit.jsont)
  |> Jsont.Object.opt_mem "media" Jsont.(list Mb_medium.jsont)
  |> Jsont.Object.finish

let artist_ids r =
  Sets.MBID.union
    (List.map Mb_artist_credit.artist_id r.artist_credits |> Sets.mbid_union)
    (List.map Mb_medium.artist_ids r.media |> Sets.mbid_union)

let update_artists map r =
  let open Result.Syntax in
  let* artist_credits = Result_list.map (Mb_artist_credit.update_artist map) r.artist_credits
  and* media = Result_list.map (Mb_medium.update_artists map) r.media in
  Ok { r with artist_credits; media }
