(* mb_raw.ml -- part of PML (Physical Media Library)

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

let jsont =
  Jsont.Object.map Fun.id
  |> Jsont.Object.keep_unknown Jsont.json_mems ~enc:Fun.id
  |> Jsont.Object.finish

let compare_members ((name1, _), _) ((name2, _), _) =
  String.compare name1 name2

let sort_members members =
  List.sort compare_members members

let map_members f members =
  List.map (fun (name, json) -> (name, f json)) members

let rec sort_json = function
  | Jsont.Object (members, meta) ->
     Jsont.Object (sort_members (map_members sort_json members), meta)
  | Jsont.Array (members, meta) ->
     Jsont.Array (List.map sort_json members, meta)
  | atom -> atom

let of_file name =
  In_channel.with_open_text name In_channel.input_all
  |> Jsont_bytesrw.decode_string jsont

let print_json json =
  match Jsont_bytesrw.encode_string ~format:Jsont.Indent jsont (sort_json json) with
  | Ok text -> print_endline text
  | Error msg -> prerr_endline msg

let print_file name =
  match of_file name with
  | Ok json -> print_json json
  | Error msg -> prerr_endline msg

let normalize text =
  let open Result.Syntax in
  let* json = Jsont_bytesrw.decode_string jsont text in
  Jsont_bytesrw.encode_string ~format:Jsont.Indent jsont (sort_json json)

let rec walk f path = function
  | Jsont.Null ((), _meta) -> ()
  | Jsont.Bool (_, _meta) -> ()
  | Jsont.Number (_, _meta) -> ()
  | Jsont.String (s, _meta) -> f path s
  | Jsont.Array (a, _meta) -> List.iter (walk f path) a
  | Jsont.Object (members, _meta) ->
     List.iter (fun ((name, _meta), json) -> walk f (name :: path) json) members

let _walk_file f path name =
  let open Result.Syntax in
  let* json = of_file name in
  Ok (walk f path json)

let grep ~rex root text =
  let open Result.Syntax in
  let print_match path s =
    if Pcre2.pmatch ~rex s then
      Printf.printf "%s: %s\n" (List.rev path |> String.concat ".") s in
  let* json = Jsont_bytesrw.decode_string jsont text in
  Ok (walk print_match root json)

let collect empty leaf node path json =
  let rec collect' path = function
    | Jsont.Null ((), _meta) -> empty
    | Jsont.Bool (_, _meta) -> empty
    | Jsont.Number (_, _meta) -> empty
    | Jsont.String (s, _meta) -> leaf path s
    | Jsont.Array (a, _meta) -> node path (List.map (collect' path) a)
    | Jsont.Object (members, _meta) ->
       node path (List.map (fun ((name, _meta), json) -> collect' (name :: path) json) members) in
  collect' path json

module MMap = Map.Make (String)
module PSet = Set.Make (struct type t = string list let compare = List.compare String.compare end)

let matches ~rex root text =
  let open Result.Syntax in
  let empty = MMap.empty
  and grab_match path s =
    if Pcre2.pmatch ~rex s then
      MMap.singleton s (PSet.singleton path)
    else
      MMap.empty
  and combine_match_pair map1 map2 =
    MMap.union (fun _key paths1 paths2 -> Some (PSet.union paths1 paths2)) map1 map2 in
  let combine_matches _path maps =
    List.fold_left combine_match_pair empty maps in
  let* json = Jsont_bytesrw.decode_string jsont text in
  Ok (collect empty grab_match combine_matches root json)

let indent pfx = pfx ^ "  "

let rec dump_schema_json pfx = function
  | Jsont.Object (mem_list, _meta) ->
     List.iter (dump_schema_mem pfx) mem_list
  | Jsont.Array ([], _meta) -> ()
  | Jsont.Array (json :: _ as json_list, _meta) ->
     Printf.printf "%s%d*\n" pfx (List.length json_list);
     dump_schema_json (indent pfx) json
  | _ -> ()

and dump_schema_mem pfx ((name, _meta), json) =
  Printf.printf "%s%s\n" pfx name;
  dump_schema_json (indent pfx) json

let dump_schema json =
  dump_schema_json "" json

let dump_schema json =
  Printexc.print dump_schema json

let dump_schema_file name =
  match of_file name with
  | Ok json -> dump_schema json
  | Error msg -> prerr_endline msg
