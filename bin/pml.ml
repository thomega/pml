(* pml.ml -- part of PML (Physical Media Library)

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

let default_cache =
  match Sys.getenv_opt "HOME" with
  | Some home -> Filename.concat home ".local/share/pml/cache"
  | None -> "pml-cache"

open Pml
open Cmdliner
open Cmdliner.Term.Syntax

module type Common =
  sig
    val man_footer : Manpage.block list
  end

module Common : Common =
  struct 

    let man_footer =
      [ `S Manpage.s_files;
        `I ("$(b," ^ default_cache ^ ")",
            "Directory containg cached JSON responses from MusicBrainz.");
        `S Manpage.s_authors;
        `P "Thorsten Ohl <ohl@physik.uni-wuerzburg.de>.";
        `S Manpage.s_bugs;
        `P "Report bugs to <ohl@physik.uni-wuerzburg.de>.";
        `P "Needs more testing on different discs.
            The command line interface is still messy." ]

  end

let exit_result = function
  | Error msg -> prerr_endline msg; 1
  | Ok () -> 0

module type Exit_Cmd =
  sig
    val cmd : int Cmd.t
  end

let root =
  let doc = Printf.sprintf "Path to the root directory of the local cache."
  and env = Cmd.Env.info "MUSICBRAINZ_CACHE" in
  Arg.(value & opt dirpath default_cache & info ["cache"] ~docv:"path" ~doc ~env)

module Init : Exit_Cmd =
  struct

    let man = [
        `S Manpage.s_description;
        `P "Initialize the cache." ] @ Common.man_footer

    let cmd =
      let open Cmd in
      make (info "init" ~man) @@
        let+ root in
        Cached.init ~root |> exit_result

  end

module Cachetest : Exit_Cmd =
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
        |> exit_result

  end

module Grep : Exit_Cmd =
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
        |> exit_result

  end

module JSON : Exit_Cmd =
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
        try Json.dump_schema_file file; 0 with _ -> 1
      else if pretty then
        try Json.print_file file; 0 with _ -> 1
      else
        0

    let cmd =
      let open Cmd in
      make (info "json" ~man) @@
        let+ root and+ file and+ schema and+ pretty in
        parse_json ~root ~file ~schema ~pretty ()

  end

let default_device =
  Libdiscid.default_device ()

let device =
  let doc = Printf.sprintf "Choose CD-ROM device." in
  Arg.(value & opt filepath default_device & info ["device"] ~doc)

let discid =
  let doc = Printf.sprintf "Discid of the disc to be examined, if there is
                            no disc in the drive. The discid is the output
                            of $(b,pml discid --id)." in
  Arg.(value & pos 0 (some string) None & info [] ~docv:"discid" ~doc)

let title =
  let doc = Printf.sprintf "Overwrite derived title." in
  Arg.(value & opt (some string) None & info ["t"; "title"] ~docv:"title" ~doc)

let perl_s =
  let parser = Edit.perl_s_of_string in
  let pp ppf sub = Format.pp_print_string ppf (Edit.perl_s_to_string sub) in
  Cmdliner.Arg.Conv.make ~docv:"/regexp/substitution/flags" ~parser ~pp ()

let edit_prefix =
  let doc = Printf.sprintf "Edit the common prefix with a pair
                            of perl regular expression and substitution
                            string.  E.g. $(b,--edit_prefix '/:.*$//') will
                            chop off everything after a colon.  This
                            is helpful, if all track portions start with
                            the same letter, for example if the movements
                            of a classical piece are enumerated my roman
                            numerals and there are fewer that five movements.
                            Note that the '/' can be replaced by any other
                            character, but it can not be escaped by '\\\\'
                            in the expressions.  The only flags accepted
                            are 'i' for case insensitive and 'g' for
                            repeated matching." in
  Arg.(value & opt (some perl_s) None & info ["edit_prefix"] ~doc)

let edit_title =
  let doc = Printf.sprintf "Edit the title after all other edits. This does
                            $(b,not) affect the extraction of the common
                            prefix, but allows to normalize directory names.
                            E.g. if there are titles containing single and
                            double digit numbers,
                            $(b,--edit_title '/ (\\\\d\\) / 0\\$1 /') will add a
                            leading zero to single digits for simpler sorting.
                            Note that the '/' can be replaced by any other
                            character, but it can not be escaped by '\\\\'
                            in the expressions.  The only flags accepted
                            are 'i' for case insensitive and 'g' for
                            repeated matching." in
  Arg.(value & opt (some perl_s) None & info ["edit_title"] ~doc)

let recording_titles =
  let doc = "Choose the recording titles as track titles." in
  Arg.(value & flag & info ["R"; "recording"] ~doc)

let medium_title =
  let doc = "Choose the medium title." in
  Arg.(value & flag & info ["m"; "medium"] ~doc)

let release_title =
  let doc = "Choose the release title." in
  Arg.(value & flag & info ["r"; "release"] ~doc)

let composer =
  let doc = Printf.sprintf "Overwrite derived composer (top billing)." in
  Arg.(value & opt (some string) None & info ["c"; "composer"] ~docv:"name" ~doc)

let performer =
  let doc = Printf.sprintf "Overwrite derived performer (top billing)." in
  Arg.(value & opt (some string) None & info ["p"; "performer"] ~docv:"name" ~doc)

let composer_prefix =
  let doc = Printf.sprintf "Overwrite derived composer (top billing) by matching prefix." in
  Arg.(value & opt (some string) None & info ["C"; "Composer"] ~docv:"prefix" ~doc)

let performer_prefix =
  let doc = Printf.sprintf "Overwrite derived performer (top billing) by matching prefix." in
  Arg.(value & opt (some string) None & info ["P"; "Performer"] ~docv:"prefix" ~doc)

let offset =
  let doc = Printf.sprintf "Apply an offset to the track numbers." in
  Arg.(value & opt int Tagged.(default_trackset.offset) & info ["o"; "offset"] ~docv:"n" ~doc)

let first =
  let doc = Printf.sprintf "First track to select (counting from 1,
                            $(b,before) applying offset)." in
  Arg.(value & opt int Tagged.(default_trackset.first) & info ["f"; "first"] ~docv:"n" ~doc)

let last =
  let doc = Printf.sprintf "Last track to select (counting from 1,
                            $(b,before) applying offset)." in
  Arg.(value & opt (some int) Tagged.(default_trackset.last) & info ["l"; "last"] ~docv:"n" ~doc)

let width =
  let doc = Printf.sprintf "The width of the printed track number,
                            including leading zeros." in
  Arg.(value & opt int Tagged.(default_trackset.width) & info ["w"; "width"] ~docv:"n" ~doc)

let trackset =
  let+ offset and+ first and+ last and+ width in
  let ts = Tagged.{ offset; first; last; width } in
  if ts = Tagged.default_trackset then
    None
  else
    Some ts

type editing =
  { title : string option;
    edit_prefix : Edit.perl_s option;
    edit_title : Edit.perl_s option;
    recording_titles : bool;
    medium_title : bool;
    release_title : bool;
    composer : string option;
    composer_prefix : string option;
    performer : string option;
    performer_prefix : string option;
    trackset : Tagged.trackset option }

let editing =
  let+ title and+ edit_prefix and+ edit_title
     and+ recording_titles and+ release_title and+ medium_title
     and+ composer and+ composer_prefix and+ performer and+ performer_prefix
     and+ trackset in
  { title; edit_prefix; edit_title; recording_titles; release_title; medium_title;
    composer; composer_prefix; performer; performer_prefix; trackset }

let apply_edit f string_opt tagged =
  let open Result.Syntax in
  let* tagged in
  match string_opt with
  | None -> Ok tagged
  | Some s -> f s tagged

let apply_edit_if flag f tagged =
  let open Result.Syntax in
  let* tagged in
  if flag then
    f tagged
  else
    Ok tagged

let apply_pcre f pcre_opt tagged =
  let open Result.Syntax in
  let* tagged in
  match pcre_opt with
  | None -> Ok tagged
  | Some sub -> f sub tagged

(** The order is very significant! *)
let apply_edits e tagged =
  Ok tagged
  |> apply_edit Tagged.select_tracks e.trackset
  |> apply_edit_if e.recording_titles Tagged.recording_titles
  |> apply_edit_if e.release_title Tagged.release_title
  |> apply_edit_if e.medium_title Tagged.medium_title
  |> apply_edit Tagged.user_title e.title
  |> apply_pcre Tagged.edit_prefix e.edit_prefix
  |> apply_pcre Tagged.edit_title e.edit_title
  |> apply_edit Tagged.composer_prefix e.composer_prefix
  |> apply_edit Tagged.performer_prefix e.performer_prefix
  |> apply_edit Tagged.user_composer e.composer
  |> apply_edit Tagged.user_performer e.performer

let medium =
  let doc =
    Printf.sprintf "Select the medium with MBID matching this prefix,
                    in case of an ambiguous discid pointing to more
                    than one media in the MusicBrainz database." in
  Arg.(value & opt (some string) None & info ["M"; "medium_id"] ~docv:"MBID" ~doc)

let get_discid ?device ?discid () =
  let open Result.Syntax in
  match discid with
  | Some discid -> Ok discid
  | None ->
     let* ids = Libdiscid.get ?device () in
     Ok (ids.Libdiscid.id)

module Medium : Exit_Cmd =
  struct

    let man = [
        `S Manpage.s_description;
        `P "Show the information relevant for tagging a medium
            in the context of a release as returned by MusicBrainz
            without any editing or interpretation." ] @ Common.man_footer

    let f ~root ?medium ?discid ?device () =
      let open Result.Syntax in
      let* id = get_discid ?device ?discid () in
      let* disc = Taggable.of_discid ~root ?medium id in
      Ok (Taggable.print disc)

    let cmd =
      let open Cmd in
      make (info "medium" ~man) @@
        let+ root and+ medium and+ discid and+ device in
        f ~root ?medium ?discid ~device ()
        |> exit_result

  end

module Editor : Exit_Cmd =
  struct

    let man = [
        `S Manpage.s_description;
        `P "Explore editing options for the information on
            a medium returned by MusicBrainz in order to
            fine tune the tagging.";
        `P "This subcommand accepts the same editing options
            as the $(b,rip) subcommand." ] @ Common.man_footer

    let no_artists =
      let doc = "Don't print the artists to be able to focus on
                 editing the titles and top billed performers." in
      Arg.(value & flag & info ["A"; "no_artists"] ~doc)

    let factor_artists =
      let doc = "Move artists common to all tracks to the disk
                 and show only the remaining ones with the tracks." in
      Arg.(value & flag & info ["F"; "factor_artists"] ~doc)

    let no_originals =
      let doc = "Don't print the unedited track titles
                 from MusicBrainz." in
      Arg.(value & flag & info ["O"; "no_originals"] ~doc)

    let no_recordings =
      let doc = "Don't print the recording titles
                 from MusicBrainz." in
      Arg.(value & flag & info ["N"; "no_recordings"] ~doc)

    let f ~root ?medium ?discid ?device ?no_artists ?factor_artists
          ?no_originals ?no_recordings ~editing () =
      let open Result.Syntax in
      let* id = get_discid ?device ?discid () in
      let* disc = Taggable.of_discid ~root ?medium id in
      let* tagged = apply_edits editing (Tagged.of_mb disc) in
      Ok (Tagged.print ?no_artists ?factor_artists ?no_originals ?no_recordings tagged)

    let cmd =
      let open Cmd in
      make (info "edit" ~man) @@
        let+ root and+ medium and+ discid and+ device
           and+ no_artists and+ factor_artists
           and+ no_originals and+ no_recordings and+ editing in
        f ~root ?medium ?discid ~device ~no_artists ~factor_artists
          ~no_originals ~no_recordings ~editing ()
        |> exit_result

  end

module Ripper : Exit_Cmd =
  struct

    let man = [
        `S Manpage.s_description;
        `P "Rip and encode tracks from a CD.";
        `P "The $(b,edit) subcommand accepts the same editing
            options as this subcommand and should be used to
            fine tune them before ripping and encoding." ] @ Common.man_footer

    let dry =
      let doc = "Don't execute external programs." in
      Arg.(value & flag & info ["d"; "dryrun"] ~doc)

    let verbose =
      let doc = "Echo command lines of execute external programs." in
      Arg.(value & flag & info ["v"; "verbose"] ~doc)

    let directory =
      let doc = "The directory to execute the external programs in." in
      Arg.(value & opt (some dirpath) None & info ["D"; "dir"] ~docv:"name" ~doc)

    let default_bitrate = 128

    let bitrate =
      let doc = "Choose bitrate." in
      Arg.(value & opt int default_bitrate & info ["b"; "bitrate"] ~docv:"bits/sec" ~doc)

    let encoders =
      let doc = "$(docv) select encoders from the list [" ^
                  String.concat ", " (List.map Pml.Rip.encoder_to_string Pml.Rip.encoders) ^
                    "]. The default is \"opus\"." in
      let enc_enum =
        List.map (fun enc -> (Pml.Rip.encoder_to_string enc, enc)) Pml.Rip.encoders in
      Arg.(value & opt_all (enum enc_enum) [Pml.Rip.Opus] & info ["e"; "encoder"] ~doc ~docv:"encoder")

    module ESet = Set.Make (struct type t = Pml.Rip.encoder let compare = Stdlib.compare end)

    let f ~root ?medium ?discid ?device ?directory ~dry ~verbose ~editing
          ~bitrate ~encoders () =
      let open Result.Syntax in
      let* id = get_discid ?device ?discid () in
      let* disc = Taggable.of_discid ~root ?medium id in
      let* tagged = apply_edits editing (Tagged.of_mb disc) in
      let encoders = ESet.of_list encoders |> ESet.elements in
      Rip.execute ~dry ~verbose ?directory ~bitrate encoders tagged

    let cmd =
      let open Cmd in
      make (info "rip" ~man) @@
        let+ dry and+ verbose and+ directory
           and+ root and+ medium and+ discid and+ device
           and+ bitrate and+ encoders and+ editing in
        f ~root ~dry ~verbose ?directory ?medium ?discid ~device ~bitrate ~encoders ~editing ()
        |> exit_result

  end

module Disc : Exit_Cmd =
  struct

    let man = [
        `S Manpage.s_description;
        `P "Query the disc in the drive." ] @ Common.man_footer

    let verbose =
      let doc = "Be more verbose." in
      Arg.(value & flag & info ["v"; "verbose"] ~doc)

    let lookup =
      let doc = "Lookup the discid as recognized by MusicBrainz." in
      Arg.(value & flag & info ["l"; "lookup"] ~doc)

    let print_id =
      let doc = "Print the discid as recognized by MusicBrainz." in
      Arg.(value & flag & info ["i"; "id"] ~doc)

    let print_toc =
      let doc = "Print the table of contents of the discid as used
                 for fuzzy searches on MusicBrainz." in
      Arg.(value & flag & info ["t"; "toc"] ~doc)

    let print_submission_url =
      let doc = "Print the URL accessing the discid submission interface." in
      Arg.(value & flag & info ["u"; "url"] ~doc)

    let f ~device ~verbose ~root ~lookup ~print_id ~print_toc ~print_submission_url =
      let open Result.Syntax in
      if verbose then
        Printf.printf "querying %s ...\n" device;
      let* ids = Libdiscid.get ~device () in
      if lookup then
        let* json = Cached.Discid.get ~root ids.id in
        Printf.printf
               "received %d bytes for %s in %s/discid\n"
               (String.length json) ids.id root;
        Ok ()
      else if print_id then
        Ok (print_endline ids.id)
      else if print_toc then
        Ok (print_endline ids.toc)
      else if print_submission_url then
        Ok (print_endline ids.submission_url)
      else
        Ok (Printf.printf "id = %s\ntoc = %s\nsubmit = %s\n" ids.id ids.toc ids.submission_url)
    
    let cmd =
      let open Cmd in
      make (info "discid" ~man) @@
        let+ device and+ verbose and+ root and+ lookup
           and+ print_id and+ print_toc and+ print_submission_url in
        f ~device ~verbose ~root ~lookup ~print_id ~print_toc ~print_submission_url
        |> exit_result

end

module Curl : Exit_Cmd =
  struct

    let man = [
        `S Manpage.s_description;
        `P "Test the curl interface." ] @ Common.man_footer

    let url =
      let doc = "The URL to query." in
      Arg.(required & pos 0 (some string) None & info [] ~docv:"url" ~doc)

    let default_user_agent =
      Version.long_name ^ " [Test] / " ^ Version.version ^ " ( " ^ Version.email ^ " )"

    let user_agent =
      let doc = "The user agent string." in
      Arg.(value & opt string default_user_agent & info ["u"; "user"] ~doc)

    let timeout =
      let doc = "Timeout for query." in
      Arg.(value & opt int 0 & info ["t"; "timeout"] ~doc)

    let curl ?timeout ~user_agent url =
      match Query.curl ?timeout ~user_agent url with
      | Ok s -> print_endline "Response:"; print_endline s; 0
      | Error msg -> prerr_endline "Error:"; prerr_endline msg; 1

    let cmd =
      let open Cmd in
      make (info "curl" ~man) @@
        let+ timeout and+ user_agent and+ url in
        curl ~timeout ~user_agent url

end

module Version : Exit_Cmd =
  struct

    let man = [
        `S Manpage.s_description;
        `P "Print version information." ] @ Common.man_footer

    let license =
      let doc = "Print license information." in
      Arg.(value & flag & info ["l"; "license"] ~doc)

    let version ~license =
      let open Version in
      if license then
        let gpl =
          "you can redistribute
it and/or modify it under the terms of the GNU General Public
License as published by the Free Software Foundation, either
version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
             
You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>." in
        Printf.printf "Copyright (C) %s %s <%s>\n\n" copyright author email;
        Printf.printf "%s (%s) is free software: %s\n" name long_name gpl
      else
        print_endline version;
      0
    
    let cmd =
      let open Cmd in
      make (info "version" ~man) @@
        let+ license in
        version ~license

end

module Main : Exit_Cmd =
  struct

    let _doc = "Physical Media Library"
    
    let man =
      [ `S Manpage.s_synopsis;
        `P "$(b,pml) [$(i,OPTIONS)]";
        `S Manpage.s_description;
        `P "Query the CD-ROM and the MusicBrainz database.";
        `S Manpage.s_authors;
        `P "Thorsten Ohl <ohl@physik.uni-wuerzburg.de>" ] @ Common.man_footer

    let cmd =
      let open Cmd in
      group (info "pml" ~man)
        [ Disc.cmd;
          JSON.cmd;
          Medium.cmd;
          Editor.cmd;
          Ripper.cmd;
          Grep.cmd;
          Init.cmd;
          Cachetest.cmd;
          Curl.cmd;
          Version.cmd]

  end

let main () =
  Cmd.eval' Main.cmd

let () =
  if !Sys.interactive then
    ()
  else
    exit (main ())
