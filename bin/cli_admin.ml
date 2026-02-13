(* pml_admin.ml -- part of PML (Physical Media Library)

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

open Pml_lib
open Cli_common

open Cmdliner
open Cmdliner.Term.Syntax

module Init : Unit_Result_Cmd =
  struct

    let man = [
        `S Manpage.s_description;
        `P "Initialize the cache." ] @ Common.man_footer

    let cmd =
      let open Cmd in
      make (info "init" ~man) @@
        let+ root in
        Cached.init ~root

  end

module Cachetest : Unit_Result_Cmd =
  struct

    let man = [
        `S Manpage.s_description;
        `P "Test and explore the cache." ] @ Common.man_footer

    let print =
      let doc = "Print the JSON file." in
      Arg.(value & flag & info ["p"; "print"] ~doc)

    let normalize =
      let doc = "Normalize the JSON file." in
      Arg.(value & flag & info ["n"; "normalize"] ~doc)

    let remote =
      let doc = "Access the remote database, iff necessary." in
      Arg.(value & flag & info ["remote"] ~doc)

    let discid =
      let doc = Printf.sprintf "Lookup by discid." in
      Arg.(value & opt (some string) None & info ["d"; "discid"] ~docv:"discid" ~doc)

    let discid_list =
      let doc = Printf.sprintf "List all cached discids." in
      Arg.(value & flag & info ["D"; "list_discids"] ~doc)

    let release =
      let doc = Printf.sprintf "Lookup by release." in
      Arg.(value & opt (some string) None & info ["r"; "release"] ~docv:"release" ~doc)

    let release_list =
      let doc = Printf.sprintf "List all cached releases." in
      Arg.(value & flag & info ["R"; "list_releases"] ~doc)

    let artist =
      let doc = Printf.sprintf "Lookup by artist." in
      Arg.(value & opt (some string) None & info ["a"; "artist"] ~docv:"artist" ~doc)

    let artist_list =
      let doc = Printf.sprintf "List all cached artists." in
      Arg.(value & flag & info ["A"; "list_artists"] ~doc)

    let print_keys key_value_pairs =
      List.iter (fun (key, _) -> print_endline key) key_value_pairs

    let found print kind key = function
      | Ok (Some json) ->
         begin match Mb_error.get_error_opt json with
         | None -> if print then print_endline json; Ok ()
         | Some msg -> Error (Printf.sprintf "%s key '%s' points to error object: '%s'" kind key msg)
         end
      | Ok None -> Error (Printf.sprintf "%s key '%s': not found locally" kind key)
      | Error _ as e -> e

    let f ~root ~normalize ~remote ~print ?discid ?release ?artist
          ~discid_list ~release_list ~artist_list () =
      let open Result.Syntax in
      if discid_list then
        let* discids = Cached.Discid.all_local ~root in
        Ok (print_keys discids)
      else if release_list then
        let* releases = Cached.Release.all_local ~root in
        Ok (print_keys releases)
      else if artist_list then
        let* artists = Cached.Artist.all_local ~root in
        Ok (print_keys artists)
      else
        let* _ =
          match discid with
          | Some discid ->
             if normalize then
               Cached.Discid.Internal.map ~root discid Json.normalize
             else if remote then
               Cached.Discid.get ~root discid |> Result.map (fun _ -> ())
             else
               Cached.Discid.local ~root discid |> found print "discid" discid
          | None -> Ok ()
        and* _ =
          match release with
          | Some release ->
             if normalize then
               Cached.Release.Internal.map ~root release Json.normalize
             else if remote then
               Cached.Release.get ~root release |> Result.map (fun _ -> ())
             else
               Cached.Release.local ~root release |> found print "release" release
          | None -> Ok ()
        and* _ =
          match artist with
          | Some artist ->
             if normalize then
               Cached.Artist.Internal.map ~root artist Json.normalize
             else if remote then
               Cached.Artist.get ~root artist |> Result.map (fun _ -> ())
             else
               Cached.Artist.local ~root artist |> found print "artist" artist
          | None -> Ok () in
        Ok ()

    let cmd =
      let open Cmd in
      make (info "cache" ~man) @@
        let+ root and+ normalize and+ remote and+ print and+ discid and+ release and+ artist
           and+ discid_list and+ release_list and+ artist_list in
        f ~root ~normalize ~remote ~print ?discid ?release ?artist
          ~discid_list ~release_list ~artist_list ()

  end

module Grep : Unit_Result_Cmd =
  struct

    let man = [
        `S Manpage.s_description;
        `P "Search for strings in the local cache." ] @ Common.man_footer

    let caseless =
      let doc = Printf.sprintf "Match case insensitively." in
      Arg.(value & flag & info ["i"; "caseless"] ~doc)

    let regexp =
      let doc = "Regular expression (PCRE2 syntax)." in
      Arg.(value & pos 0 (some string) None & info [] ~docv:"regexp" ~doc)

    let long =
      let doc = Printf.sprintf "Print the complete paths." in
      Arg.(value & flag & info ["l"; "long"] ~doc)

    let short =
      let doc = Printf.sprintf "Only keep filenames in output paths." in
      Arg.(value & flag & info ["s"; "short"] ~doc)

    let combine_match_pair map1 map2 =
      Json.MMap.union (fun _key paths1 paths2 -> Some (Json.PSet.union paths1 paths2)) map1 map2

    let combine_matches maps =
      List.fold_left combine_match_pair Json.MMap.empty maps

    let grep1 ~rex name entries =
      let open Result.Syntax in
      let* matches =
        Result_list.map (fun (id, json) -> Json.matches ~rex [name ^ "/" ^ id] json) entries in
      Ok (combine_matches matches)

    let print_paths paths =
      Json.PSet.to_list paths
      |> List.map List.rev
      |> List.sort (List.compare String.compare)
      |> List.iter (fun path -> Printf.printf "  %s\n" (String.concat "." path))

    let print_matches matches =
      Json.MMap.iter
        (fun m paths ->
          Printf.printf "%s:\n" m;
          print_paths paths)
        matches

    let shorten_path = function
      | [] | [_] | [_; _] as path -> path
      | a :: path -> [a; "."; List.hd (List.rev path)]

    let shorten_paths = Json.MMap.map (Json.PSet.map shorten_path)

    let chop_path = function
      | [] -> []
      | _ :: path -> [List.hd (List.rev path)]

    let chop_paths = Json.MMap.map (Json.PSet.map chop_path)

    let abbreviate_paths ~short ~long paths =
      if short then
        chop_paths paths
      else if long then
        paths
      else
        shorten_paths paths

    let f ~root ~caseless ~regexp ~short ~long () =
      match regexp with
      | None -> Ok ()
      | Some regexp ->
         let open Result.Syntax in
         let flags =
           if caseless then
             [`CASELESS]
           else
             [] in
         let* rex = try Ok (Pcre2.regexp ~flags regexp) with e -> Error (Printexc.to_string e) in
         let* discids = Cached.Discid.all_local ~root in
         let* discid_matches = grep1 ~rex "discid" discids in
         let* releases = Cached.Release.all_local ~root in
         let* release_matches = grep1 ~rex "release" releases in
         let* artists = Cached.Artist.all_local ~root in
         let* artist_matches = grep1 ~rex "artist" artists in
         combine_matches [discid_matches; release_matches; artist_matches]
         |> abbreviate_paths ~short ~long
         |> print_matches;
         Ok ()

    let cmd =
      let open Cmd in
      make (info "grep" ~man) @@
        let+ root and+ caseless and+ regexp and+ short and+ long in
        f ~root ~caseless ~regexp ~short ~long ()

  end

module JSON : Unit_Result_Cmd =
  struct

    let man = [
        `S Manpage.s_description;
        `P "Parsing Musicbrainz JSON files." ] @ Common.man_footer

    let file =
      let doc = Printf.sprintf "JSON file to be examined." in
      Arg.(required & pos 0 (some filepath) None & info [] ~docv:"filename" ~doc)

    let schema =
      let doc = "Dump the whole tree without contents." in
      Arg.(value & flag & info ["s"; "schema"] ~doc)

    let pretty =
      let doc = "Dump the whole tree." in
      Arg.(value & flag & info ["p"; "pretty"] ~doc)

    let parse_json ~root ~file ~schema ~pretty () =
      ignore root;
      if schema then
        try Ok (Json.dump_schema_file file) with e -> Error (Printexc.to_string e)
      else if pretty then
        try Ok (Json.print_file file) with e -> Error (Printexc.to_string e)
      else
        Ok ()

    let cmd =
      let open Cmd in
      make (info "json" ~man) @@
        let+ root and+ file and+ schema and+ pretty in
        parse_json ~root ~file ~schema ~pretty ()

  end

module Curl : Unit_Result_Cmd =
  struct

    let man = [
        `S Manpage.s_description;
        `P "Test the curl interface." ] @ Common.man_footer

    let url =
      let doc = "The URL to query." in
      Arg.(required & pos 0 (some string) None & info [] ~docv:"url" ~doc)

    let default_user_agent =
      Config.long_name ^ " [Test] / " ^ Config.version ^ " ( " ^ Config.email ^ " )"

    let user_agent =
      let doc = "The user agent string." in
      Arg.(value & opt string default_user_agent & info ["u"; "user"] ~doc)

    let timeout =
      let doc = "Timeout for query." in
      Arg.(value & opt int 0 & info ["t"; "timeout"] ~doc)

    let curl ?timeout ~user_agent url =
      let open Result.Syntax in
      let* text = Query.curl ?timeout ~user_agent url in
      print_endline "Response:";
      print_endline text;
      Ok ()

    let cmd =
      let open Cmd in
      make (info "curl" ~man) @@
        let+ timeout and+ user_agent and+ url in
        curl ~timeout ~user_agent url

end

let man =
  [ `S Manpage.s_synopsis;
    `P "$(cmd) [$(i,OPTIONS)]";
    `S Manpage.s_description;
    `P "Manage the local cache for MusicBrainz database and debug
        the library used by the $(b,pml) command.";
    `S Manpage.s_authors;
    `P "Thorsten Ohl <ohl@physik.uni-wuerzburg.de>" ] @ Common.man_footer

let cmd =
  let open Cmd in
  group (info "admin" ~man)
    [ JSON.cmd;
      Grep.cmd;
      Init.cmd;
      Cachetest.cmd;
      Curl.cmd]
