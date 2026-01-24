let default_cache = "mb-cache"

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

let cache =
  let doc = Printf.sprintf "Path to the root directory of the local cache."
  and env = Cmd.Env.info "MUSICBRAINZ_CACHE" in
  Arg.(value & opt string default_cache & info ["c"; "cache"] ~docv:"path" ~doc ~env)

module Cachetest : Exit_Cmd =
  struct

    let man = [
        `S Manpage.s_description;
        `P "Testing." ] @ Common.man_footer

    let normalize =
      let doc = "Normalize the JSON file." in
      Arg.(value & flag & info ["n"; "normalize"] ~doc)

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

    let cache_tool ~cache ~normalize ?discid ?release ?artist
          ~discid_list ~release_list ~artist_list () =
      let module MB = Pml.Musicbrainz in
      if discid_list then
        match
          let open Result.Syntax in
          let* discids = MB.Discid_cached.all_local ~root:cache in
          Ok (List.iter (fun (discid, _) -> print_endline discid) discids)
        with
        | Error msg -> prerr_endline msg; 1
        | Ok _ -> 0
      else if release_list then
        match
          let open Result.Syntax in
          let* releases = MB.Release_cached.all_local ~root:cache in
          Ok (List.iter (fun (release, _) -> print_endline release) releases)
        with
        | Error msg -> prerr_endline msg; 1
        | Ok _ -> 0
      else if artist_list then
        match
          let open Result.Syntax in
          let* artists = MB.Artist_cached.all_local ~root:cache in
          Ok (List.iter (fun (artist, _) -> print_endline artist) artists)
        with
        | Error msg -> prerr_endline msg; 1
        | Ok _ -> 0
      else if normalize then
        let rc_discid =
          match discid with
          | None -> 0
          | Some discid ->
             begin match MB.Discid_cached.Internal.map ~root:cache discid MB.Raw.normalize with
             | Error msg -> Printf.eprintf "error: %s\n" msg; 1
             | Ok () -> 0
             end
        and rc_release =
          match release with
          | None -> 0
          | Some release ->
             begin match MB.Release_cached.Internal.map ~root:cache release MB.Raw.normalize with
             | Error msg -> Printf.eprintf "error: %s\n" msg; 1
             | Ok () -> 0
             end
        and rc_artist =
          match artist with
          | None -> 0
          | Some artist ->
             begin match MB.Artist_cached.Internal.map ~root:cache artist MB.Raw.normalize with
             | Error msg -> Printf.eprintf "error: %s\n" msg; 1
             | Ok () -> 0
             end in
        rc_discid + rc_release + rc_artist
      else
        match discid, release, artist with
        | None, None, None -> 0
        | Some discid, None, None ->
           begin match MB.Discid_cached.get ~root:cache discid with
           | Error msg -> Printf.eprintf "error: %s\n" msg; 1
           | Ok json -> print_endline json; 0
           end
        | None, Some release, None ->
           begin match MB.Release_cached.get ~root:cache release with
           | Error msg -> Printf.eprintf "error: %s\n" msg; 1
           | Ok json -> print_endline json; 0
           end
        | None, None, Some artist ->
           begin match MB.Artist_cached.get ~root:cache artist with
           | Error msg -> Printf.eprintf "error: %s\n" msg; 1
           | Ok json -> print_endline json; 0
           end
        | _ -> 1

    let cmd =
      let open Cmd in
      make (info "cache" ~man) @@
        let+ cache and+ normalize and+ discid and+ release and+ artist
           and+ discid_list and+ release_list and+ artist_list in
        cache_tool ~cache ~normalize ?discid ?release ?artist
          ~discid_list ~release_list ~artist_list ()

  end

module Musicbrainz : Exit_Cmd =
  struct

    let man = [
        `S Manpage.s_description;
        `P "Experimental Musicbrainz JSON parsing." ] @ Common.man_footer

    let file =
      let doc = Printf.sprintf "JSON file to be examined." in
      Arg.(value & opt (some string) None & info ["f"; "file"] ~docv:"name" ~doc)

    let schema =
      let doc = "Dump the whole tree without contents." in
      Arg.(value & flag & info ["s"; "schema"] ~doc)

    let pretty =
      let doc = "Dump the whole tree." in
      Arg.(value & flag & info ["p"; "pretty"] ~doc)

    let parse_json ~cache ?file ~schema ~pretty () =
      ignore cache;
      let module MB = Pml.Musicbrainz in
      match file with
      | None -> 0
      | Some name ->
         if schema then
           try MB.Raw.dump_schema_file name; 0 with _ -> 1
         else if pretty then
           try MB.Raw.print_file name; 0 with _ -> 1
         else
           0

    let cmd =
      let open Cmd in
      make (info "musicbrainz" ~man) @@
        let+ cache and+ file and+ schema and+ pretty in
        parse_json ~cache ?file ~schema ~pretty ()

  end

module Medium : Exit_Cmd =
  struct

    let man = [
        `S Manpage.s_description;
        `P "Explore media." ] @ Common.man_footer

    let discid =
      let doc = Printf.sprintf "Disc to be examined." in
      Arg.(value & opt (some string) None & info ["d"; "disc"; "discid"] ~docv:"id" ~doc)

    let processed =
      let doc = "Process the data." in
      Arg.(value & flag & info ["p"; "processed"] ~doc)

    let explore ~cache ?discid ~processed () =
      ignore cache;
      let module MB = Pml.Musicbrainz.Taggable in
      let module T = Pml.Tags.Disc in
      match discid with
      | None -> 0
      | Some id ->
         match MB.of_discid ~root:cache id with
         | Error msg -> prerr_endline msg; 1
         | Ok disc ->
            if processed then
              T.print (T.of_mb disc)
            else
              MB.print disc; 0

    let cmd =
      let open Cmd in
      make (info "medium" ~man) @@
        let+ cache and+ discid and+ processed in
        explore ~cache ?discid ~processed ()

  end

module Query_Disc : Exit_Cmd =
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

    let default_device =
      Pml.Discid.default_device ()

    let device =
      let doc = Printf.sprintf "Choose CD-ROM device." in
      Arg.(value & opt string default_device & info ["d"; "device"] ~doc)

    let query_disc ~device ~verbose ~cache ~lookup ~print_id ~print_toc ~print_submission_url =
      if verbose then
        Printf.printf "querying %s ...\n" device;
      match Pml.Discid.get ~device () with
      | Result.Ok ids ->
         begin
           if not lookup && not print_id && not print_toc && not print_submission_url then
             Printf.printf "id = %s\ntoc = %s\nsubmit = %s\n" ids.id ids.toc ids.submission_url
           else if lookup then
             begin match Pml.Musicbrainz.Discid_cached.get ~root:cache ids.id with
             | Error msg -> Printf.eprintf "error: %s\n" msg
             | Ok json ->
                Printf.printf
                  "received %d bytes for %s in %s/discid\n"
                  (String.length json) ids.id cache
             end
           else if print_id then
             print_endline ids.id
           else if print_toc then
             print_endline ids.toc
           else if print_submission_url then
             print_endline ids.submission_url
         end;
         0
      | Result.Error msg ->
         Printf.eprintf "error: %s!\n" msg;
         1

    let cmd =
      let open Cmd in
      make (info "disc" ~man) @@
        let+ device and+ verbose and+ cache and+ lookup
           and+ print_id and+ print_toc and+ print_submission_url in
        query_disc ~device ~verbose ~cache ~lookup ~print_id ~print_toc ~print_submission_url

end

module Curl : Exit_Cmd =
  struct

    let man = [
        `S Manpage.s_description;
        `P "Test the curl interface." ] @ Common.man_footer

    let url =
      let doc = "The URL to query." in
      Arg.(value & opt string "https://fritz.box" & info ["u"; "url"] ~doc)

    let user_agent =
      let doc = "The user agent string." in
      Arg.(value & opt string "pml testing" & info ["U"; "User"] ~doc)

    let timeout =
      let doc = "Timeout for query." in
      Arg.(value & opt int 0 & info ["t"; "timeout"] ~doc)

    let curl ?timeout ~user_agent url =
      match Pml.Query.curl ?timeout ~user_agent url with
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
      group (info "pml_cli" ~man)
        [ Query_Disc.cmd;
          Musicbrainz.cmd;
          Medium.cmd;
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
