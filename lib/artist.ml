(* artist.ml -- part of PML (Physical Media Library)

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
  { name : string;
    sort_name : string;
    artist_type : Artist_type.t;
    lifespan : Lifespan.t;
    id : string }

let set_white =
  Re.set " \t\n\r"

let set_not_white =
  Re.(compl [set_white])

let set_comma =
  Re.set ","

let set_not_comma =
  Re.(compl [set_comma])

let set_not_comma_white =
  Re.(compl [set_comma; set_white])

let re_name =
  Re.(seq [start;
           rep set_white;
           group (alt [rep1 set_not_white;
                       seq [set_not_white; rep any; set_not_white]]);
           rep1 set_white;
           group (rep1 set_not_white);
           rep set_white;
           stop] |> compile)

let sort_name_of_name name =
  match Re.exec_opt re_name name with
  | None -> name
  | Some groups ->
     let first = Re.Group.get groups 1
     and last = Re.Group.get groups 2 in
     last ^ ", " ^ first

let%test _ = sort_name_of_name "a b" = "b, a"
let%test _ = sort_name_of_name " a b c " = "c, a b"
let%test _ = sort_name_of_name "ab" = "ab"

let re_sort_name =
  Re.(seq [start;
           rep set_white;
           group (alt [set_not_comma_white;
                       seq [set_not_comma_white; rep set_not_comma; set_not_comma_white]]);
           rep set_white;
           rep set_comma;
           rep set_white;
           group (seq [rep set_not_comma; set_not_comma_white]);
           rep set_white;
           stop] |> compile)

let name_of_sort_name sort_name =
  match Re.exec_opt re_sort_name sort_name with
  | None -> sort_name
  | Some groups ->
     let last = Re.Group.get groups 1
     and first = Re.Group.get groups 2 in
     first ^ " " ^ last

let%test _ = name_of_sort_name "a, b" = "b a"
let%test _ = name_of_sort_name "a, b " = "b a"
let%test _ = name_of_sort_name " a, b " = "b a"
let%test _ = name_of_sort_name " a , b " = "b a"

let compare a1 a2 =
  let c = Artist_type.compare a1.artist_type a2.artist_type in
  if c <> 0 then
    c
  else
    let c = Lifespan.compare a1.lifespan a2.lifespan in
    if c <> 0 then
      c
    else
      let c = String.compare a1.name a2.name in
      if c <> 0 then
        c
      else
        String.compare a1.id a2.id

let of_mb mb =
  let module T = Artist_type in
  let id = mb.Mb_artist.id
  and name, sort_name =
    match mb.Mb_artist.name, mb.Mb_artist.sort_name with
    | Some name, Some sort_name -> name, sort_name
    | Some name, None -> name, sort_name_of_name name
    | None, Some sort_name -> name_of_sort_name sort_name, sort_name
    | None, None -> "Anonymous", "Anonymous"
  and artist_type =
    Option.value mb.Mb_artist.artist_type ~default:(T.Person T.Roles.empty)
  and lifespan =
    Option.value mb.Mb_artist.lifespan ~default:Lifespan.Limbo in
  { id; name; sort_name; artist_type; lifespan }

let of_name name =
  let sort_name = sort_name_of_name name
  and artist_type = Artist_type.(Person Roles.empty)
  and lifespan = Lifespan.Limbo
  and id = "" in
  {name; sort_name; artist_type; lifespan; id }

let of_sort_name sort_name =
  let name = name_of_sort_name sort_name
  and artist_type = Artist_type.(Person Roles.empty)
  and lifespan = Lifespan.Limbo
  and id = "" in
  {name; sort_name; artist_type; lifespan; id }

let of_name_sort_name name sort_name =
  let artist_type = Artist_type.(Person Roles.empty)
  and lifespan = Lifespan.Limbo
  and id = "" in
  {name; sort_name; artist_type; lifespan; id }

type artist_t = t
(** We can't write [type t = t] below. *)

module Collection = Set.Make (struct type t = artist_t let compare = compare end)

let find_gaps is_gap sorted_list =
  let open List in
  let rec find_gaps' groups_rev group_rev = function
    | [] -> rev_append groups_rev [rev group_rev]
    | [_] as a -> rev_append groups_rev [rev_append group_rev a]
    | a1 :: (a2 :: _ as a2_etc) ->
       if is_gap a1 a2 then
         find_gaps' (rev_append group_rev [a1] :: groups_rev) [] a2_etc
       else
         find_gaps' groups_rev (a1 :: group_rev) a2_etc in
  find_gaps' [] [] sorted_list

let%test_module _ =
  (module struct

     let is_gap x y = y > x + 1

     let test_gaps alist groups =
       find_gaps is_gap alist = groups

     let%test _ = test_gaps [] [[]]
     let%test _ = test_gaps [1] [[1]]
     let%test _ = test_gaps [1;2] [[1;2]]
     let%test _ = test_gaps [1;3] [[1];[3]]
     let%test _ = test_gaps [1;3;5] [[1];[3];[5]]
     let%test _ = test_gaps [1;2;4;5;6;8;9] [[1;2];[4;5;6];[8;9]]
     let%test _ = test_gaps [1;3;2] [[1];[3;2]]

   end)

let lifespan_gaps artists =
  Collection.elements artists
  |> List.sort (fun a1 a2 -> Lifespan.compare a1.lifespan a2.lifespan)
  |> find_gaps
       (fun a1 a2 ->
         match Lifespan.relation a1.lifespan a2.lifespan with
         | Before -> true
         | After | Overlap -> false)
  |> List.map Collection.of_list

let of_credits credits =
  List.filter_map (fun c -> Option.map of_mb c.Mb_artist_credit.artist) credits
  |> Collection.of_list

let to_string ?(sortable=false) a =
  let name =
    if sortable then
      a.sort_name
    else
      a.name
  and artist_type =
    match Artist_type.to_string_opt a.artist_type with
    | None -> ""
    | Some s -> " (" ^ s ^ ")"
  and lifespan =
    match Lifespan.to_string_opt a.lifespan with
    | None -> ""
    | Some lifespan -> " [" ^ lifespan ^ "]" in
  name ^ artist_type ^ lifespan
