(* mb_artist.ml -- part of PML (Physical Media Library)

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
    name : string option;
    sort_name : string option;
    artist_type : Artist_type.t option;
    lifespan : Lifespan.t option;
    disambiguation : string option }

let make id name sort_name artist_type lifespan disambiguation =
  let name = Edit.blank_to_none name
  and sort_name = Edit.blank_to_none sort_name in
  let roles =
    match disambiguation with
    | None -> Artist_type.no_role
    | Some disambiguation -> Artist_type.roles_of_string disambiguation in
  let artist_type = Option.map (Artist_type.of_string roles) artist_type in
  { id; name; sort_name; artist_type; lifespan; disambiguation }

let jsont =
  Jsont.Object.map ~kind:"Artist" make
  |> Jsont.Object.mem "id" Jsont.string
  |> Jsont.Object.opt_mem "name" Jsont.string
  |> Jsont.Object.opt_mem "sort-name" Jsont.string
  |> Jsont.Object.opt_mem "type" Jsont.string
  |> Jsont.Object.opt_mem "life-span" Lifespan.jsont
  |> Jsont.Object.opt_mem "disambiguation" Jsont.string
  |> Jsont.Object.finish

let id a = Sets.MBID.singleton a.id

let update map a =
  (* List.iter (fun (s, _) -> Printf.printf "key: %s\n" s) (Cached.Artist.M.bindings map); *)
  match Cached.Artist.M.find_opt a.id map with
  | Some a -> Ok a
  | None -> Error (Printf.sprintf "Artist ID '%s' not found!" a.id)

let compare a1 a2 =
  let c =
    match a1.artist_type, a2.artist_type with
    | Some at1, Some at2 -> Artist_type.compare at1 at2
    | Some _, None -> -1
    | None, Some _ -> 1
    | None, None -> 0 in
  if c <> 0 then
    c
  else
    let c = 
      match a1.lifespan, a2.lifespan with
      | Some ls1, Some ls2 -> Lifespan.compare ls1 ls2
      | Some _, None -> -1
      | None, Some _ -> 1
      | None, None -> 0 in
    if c <> 0 then
      c
    else
      let c =
        match a1.sort_name, a2.sort_name with
        | Some n1, Some n2 -> String.compare n1 n2
        | Some _, None -> -1
        | None, Some _ -> 1
        | None, None -> 0 in
      if c <> 0 then
        c
      else
        String.compare a1.id a2.id

let to_string a =
  let name =
    match a.name with
    | None -> "N.N."
    | Some name -> name
  and artist_type =
    match a.artist_type with
    | None -> ""
    | Some artist_type ->
       begin match Artist_type.to_string artist_type with
       | "" -> ""
       | s -> " (" ^ s ^ ")"
       end
  and lifespan =
    match a.lifespan with
    | None -> ""
    | Some lifespan ->
       begin match Lifespan.to_string_opt lifespan with
       | None -> ""
       | Some lifespan -> " [" ^ lifespan ^ "]"
       end in
  name ^ artist_type ^ lifespan
