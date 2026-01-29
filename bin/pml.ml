let default_cache = "mb-cache"

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
        `P "None, so far.";
        `S Manpage.s_authors;
        `P "Thorsten Ohl <ohl@physik.uni-wuerzburg.de>.";
        `S Manpage.s_bugs;
        `P "Report bugs to <ohl@physik.uni-wuerzburg.de>.";
        `P "It's not finished yet - in fact, I haven't really started." ]

  end

module type Exit_Cmd =
  sig
    val cmd : int Cmd.t
  end

let root =
  let doc = Printf.sprintf "Path to the root directory of the local cache."
  and env = Cmd.Env.info "MUSICBRAINZ_CACHE" in
  Arg.(value & opt dirpath default_cache & info ["cache"] ~docv:"path" ~doc ~env)

module Cachetest : Exit_Cmd =
  struct

    let man = [
        `S Manpage.s_description;
        `P "Testing." ] @ Common.man_footer

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

    let cache_tool ~root ~normalize ~remote ~print ?discid ?release ?artist
          ~discid_list ~release_list ~artist_list () =
      let open Result.Syntax in
      let result =
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
                 Cached.Discid.Internal.map ~root discid Mb_raw.normalize
               else if remote then
                 Cached.Discid.get ~root discid |> Result.map (fun _ -> ())
               else
                 Cached.Discid.local ~root discid |> found print "discid" discid
            | None -> Ok ()
          and* _ =
            match release with
            | Some release ->
               if normalize then
                 Cached.Release.Internal.map ~root release Mb_raw.normalize
               else if remote then
                 Cached.Release.get ~root release |> Result.map (fun _ -> ())
               else
                 Cached.Release.local ~root release |> found print "release" release
            | None -> Ok ()
          and* _ =
            match artist with
            | Some artist ->
               if normalize then
                 Cached.Artist.Internal.map ~root artist Mb_raw.normalize
               else if remote then
                 Cached.Artist.get ~root artist |> Result.map (fun _ -> ())
               else
                 Cached.Artist.local ~root artist |> found print "artist" artist
            | None -> Ok () in
          Ok () in
      match result with
      | Error msg -> prerr_endline msg; 1
      | Ok _ -> 0


    let cmd =
      let open Cmd in
      make (info "cache" ~man) @@
        let+ root and+ normalize and+ remote and+ print and+ discid and+ release and+ artist
           and+ discid_list and+ release_list and+ artist_list in
        cache_tool ~root ~normalize ~remote ~print ?discid ?release ?artist
          ~discid_list ~release_list ~artist_list ()

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
        try Mb_raw.dump_schema_file file; 0 with _ -> 1
      else if pretty then
        try Mb_raw.print_file file; 0 with _ -> 1
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
  let doc = Printf.sprintf "Disc to be examined." in
  Arg.(value & pos 0 (some string) None & info [] ~docv:"discid" ~doc)

let title =
  let doc = Printf.sprintf "Overwrite derived title." in
  Arg.(value & opt (some string) None & info ["t"; "title"] ~docv:"title" ~doc)

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
  let doc = Printf.sprintf "Overwrite derived composer (top billing)." in
  Arg.(value & opt (some string) None & info ["C"; "Composer"] ~docv:"prefix" ~doc)

let performer_prefix =
  let doc = Printf.sprintf "Overwrite derived performer (top billing)." in
  Arg.(value & opt (some string) None & info ["P"; "Performer"] ~docv:"prefix" ~doc)

let offset =
  let doc = Printf.sprintf "Apply an offset to the track numbers." in
  Arg.(value & opt int Tags.Disc.(default_trackset.offset) & info ["o"; "offset"] ~docv:"n" ~doc)

let first =
  let doc = Printf.sprintf "First track to select (counting from 1,
                            $(b,before) applying offset)." in
  Arg.(value & opt int Tags.Disc.(default_trackset.first) & info ["f"; "first"] ~docv:"n" ~doc)

let last =
  let doc = Printf.sprintf "Last track to select (counting from 1,
                            $(b,before) applying offset)." in
  Arg.(value & opt (some int) Tags.Disc.(default_trackset.last) & info ["l"; "last"] ~docv:"n" ~doc)

let width =
  let doc = Printf.sprintf "The width of the printed track number,
                            including leading zeros." in
  Arg.(value & opt int Tags.Disc.(default_trackset.width) & info ["w"; "width"] ~docv:"n" ~doc)

let trackset =
  let+ offset and+ first and+ last and+ width in
  let ts = Tags.Disc.{ offset; first; last; width } in
  if ts = Tags.Disc.default_trackset then
    None
  else
    Some ts

type editing =
  { title : string option;
    recording_titles : bool;
    medium_title : bool;
    release_title : bool;
    composer : string option;
    composer_prefix : string option;
    performer : string option;
    performer_prefix : string option;
    trackset : Tags.Disc.trackset option }

let editing =
  let+ title and+ recording_titles and+ release_title and+ medium_title
     and+ composer and+ composer_prefix and+ performer and+ performer_prefix
     and+ trackset in
  { title; recording_titles; release_title; medium_title;
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

(** The order is very significant! *)
let apply_edits e tagged =
  Ok tagged
  |> apply_edit Tags.Disc.select_tracks e.trackset
  |> apply_edit_if e.recording_titles Tags.Disc.recording_titles
  |> apply_edit_if e.release_title Tags.Disc.release_title
  |> apply_edit_if e.medium_title Tags.Disc.medium_title
  |> apply_edit Tags.Disc.user_title e.title
  |> apply_edit Tags.Disc.composer_prefix e.composer_prefix
  |> apply_edit Tags.Disc.performer_prefix e.performer_prefix
  |> apply_edit Tags.Disc.user_composer e.composer
  |> apply_edit Tags.Disc.user_performer e.performer

let medium =
  let doc =
    Printf.sprintf "Select the medium with MBID matched this prefix,
                    in case of an ambiguous diskid." in
  Arg.(value & opt (some string) None & info ["M"; "medium_id"] ~docv:"MBID" ~doc)

let get_discid ?device ?discid () =
  let open Result.Syntax in
  match discid with
  | Some discid -> Ok discid
  | None ->
     let* ids = Libdiscid.get ?device () in
     Ok (ids.Libdiscid.id)

let exit_result = function
  | Error msg -> prerr_endline msg; 1
  | Ok () -> 0

module Medium : Exit_Cmd =
  struct

    let man = [
        `S Manpage.s_description;
        `P "Show the information on a medium as returned
            by MusicBrainz." ] @ Common.man_footer

    let f ~root ?medium ?discid ?device () =
      let open Result.Syntax in
      let* id = get_discid ?device ?discid () in
      let* disc = Taggable.of_discid ~root ?medium id in
      Ok (Taggable.print disc)

    let cmd =
      let open Cmd in
      make (info "medium" ~man) @@
        let+ root and+ medium and+ discid and+ device in
        f ~root ?medium ?discid ~device () |> exit_result

  end

module Explore : Exit_Cmd =
  struct

    let man = [
        `S Manpage.s_description;
        `P "Explore editing options for the information on
            a medium returned by MusicBrainz in order to
            fine tune the tagging." ] @ Common.man_footer

    let f ~root ?medium ?discid ?device ~editing () =
      let open Result.Syntax in
      let* id = get_discid ?device ?discid () in
      let* disc = Taggable.of_discid ~root ?medium id in
      let* tagged = apply_edits editing (Tags.Disc.of_mb disc) in
      Ok (Tags.Disc.print tagged)

    let cmd =
      let open Cmd in
      make (info "explore" ~man) @@
        let+ root and+ medium and+ discid and+ device and+ editing in
        f ~root ?medium ?discid ~device ~editing () |> exit_result

  end

module Ripper : Exit_Cmd =
  struct

    let man = [
        `S Manpage.s_description;
        `P "Write a ripping/tagging script." ] @ Common.man_footer

    let f ~root ?medium ?discid ?device ~editing () =
      let open Result.Syntax in
      let* id = get_discid ?device ?discid () in
      let* disc = Taggable.of_discid ~root ?medium id in
      let* tagged = apply_edits editing (Tags.Disc.of_mb disc) in
      Tags.Disc.script tagged

    let cmd =
      let open Cmd in
      make (info "ripper" ~man) @@
        let+ root and+ medium and+ discid and+ device and+ editing in
        f ~root ?medium ?discid ~device ~editing () |> exit_result

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

    let query_disc ~device ~verbose ~root ~lookup ~print_id ~print_toc ~print_submission_url =
      if verbose then
        Printf.printf "querying %s ...\n" device;
      match Libdiscid.get ~device () with
      | Ok ids ->
         begin
           if not lookup && not print_id && not print_toc && not print_submission_url then
             Printf.printf "id = %s\ntoc = %s\nsubmit = %s\n" ids.id ids.toc ids.submission_url
           else if lookup then
             begin match Cached.Discid.get ~root ids.id with
             | Error msg -> Printf.eprintf "error: %s\n" msg
             | Ok json ->
                Printf.printf
                  "received %d bytes for %s in %s/discid\n"
                  (String.length json) ids.id root
             end
           else if print_id then
             print_endline ids.id
           else if print_toc then
             print_endline ids.toc
           else if print_submission_url then
             print_endline ids.submission_url
         end;
         0
      | Error msg ->
         Printf.eprintf "error: %s!\n" msg;
         1

    let cmd =
      let open Cmd in
      make (info "discid" ~man) @@
        let+ device and+ verbose and+ root and+ lookup
           and+ print_id and+ print_toc and+ print_submission_url in
        query_disc ~device ~verbose ~root ~lookup ~print_id ~print_toc ~print_submission_url

end

module Curl : Exit_Cmd =
  struct

    let man = [
        `S Manpage.s_description;
        `P "Test the curl interface." ] @ Common.man_footer

    let url =
      let doc = "The URL to query." in
      Arg.(required & pos 0 (some string) None & info [] ~docv:"url" ~doc)

    let user_agent =
      let doc = "The user agent string." in
      Arg.(value & opt string "pml testing" & info ["u"; "User"] ~doc)

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
          Explore.cmd;
          Ripper.cmd;
          Cachetest.cmd;
          Curl.cmd]

  end

let main () =
  Cmd.eval' Main.cmd

let () =
  if !Sys.interactive then
    ()
  else
    exit (main ())
