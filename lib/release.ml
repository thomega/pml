(* release.ml -- part of PML (Physical Media Library)

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
  { title : string option;
    artists : Artist.Collection.t;
    media : Medium.t list;
    id : string }

let of_mb mb =
  let module R = Mb_release in
  let id = mb.R.id
  and title = mb.R.title
  and artists = Artist.of_credits mb.R.artist_credits
  and media = List.map Medium.of_mb mb.R.media in
  { id; title; artists; media }

