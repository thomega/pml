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

let default_cache = "."

let cache =
  let doc = Printf.sprintf "Path to the root directory of the local cache."
  and env = Cmd.Env.info "PML_CACHE" in
  Arg.(value & opt string default_cache & info ["c"; "cache"] ~docv:"path" ~doc ~env)

module Cachetest : Exit_Cmd =
  struct

    let man = [
        `S Manpage.s_description;
        `P "Testing." ] @ Common.man_footer

    let normalize =
      let doc = "Normalize the JSON file." in
      Arg.(value & flag & info ["n"; "normalize"] ~doc)

    let diskid =
      let doc = Printf.sprintf "Lookup by diskid." in
      Arg.(value & opt (some string) None & info ["d"; "diskid"] ~docv:"diskid" ~doc)

    let release =
      let doc = Printf.sprintf "Lookup by release." in
      Arg.(value & opt (some string) None & info ["r"; "release"] ~docv:"release" ~doc)

    let cache_tool ~cache ~normalize ?diskid ?release () =
      if normalize then
        1
      else
        match diskid, release with
        | None, None -> 0
        | Some _, Some _ -> 1
        | Some diskid, None ->
           begin match Pml.Musicbrainz.get_discid_cached ~root:cache diskid with
           | Error msg -> Printf.eprintf "error: %s\n" msg; 1
           | Ok json -> print_endline json; 0
           end
        | None, Some release ->
           begin match Pml.Musicbrainz.get_release_cached ~root:cache release with
           | Error msg -> Printf.eprintf "error: %s\n" msg; 1
           | Ok json -> print_endline json; 0
           end

    let cmd =
      let open Cmd in
      make (info "cache" ~man) @@
        let+ cache and+ normalize and+ diskid and+ release in
        cache_tool ~cache ~normalize ?diskid ?release ()

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
      match file with
      | None -> 0
      | Some name ->
         if schema then
           try
             Pml.Musicbrainz.Raw.dump_schema_file name;
             0
           with
           | _ -> 1
         else if pretty then
           try
             Pml.Musicbrainz.Raw.print_file name;
             0
           with
           | _ -> 1
         else
           0

    let cmd =
      let open Cmd in
      make (info "musicbrainz" ~man) @@
        let+ cache and+ file and+ schema and+ pretty in
        parse_json ~cache ?file ~schema ~pretty ()

  end

module Query_Disc : Exit_Cmd =
  struct

    let man = [
        `S Manpage.s_description;
        `P "Query the disc in the drive." ] @ Common.man_footer

    let verbose =
      let doc = "Be more verbose." in
      Arg.(value & flag & info ["v"; "verbose"] ~doc)

    let print_id =
      let doc = "Print the discid as recognized by MusicBrainz." in
      Arg.(value & flag & info ["i"; "id"] ~doc)

    let print_freedb =
      let doc = "Print the discid as recognized by FreeDB." in
      Arg.(value & flag & info ["f"; "freedb"] ~doc)

    let print_toc =
      let doc = "Print the table of contents of the discid as used
                 for fuzzy searches on MusicBrainz." in
      Arg.(value & flag & info ["t"; "toc"] ~doc)

    let default_device =
      Pml.Discid.default_device ()

    let device =
      let doc = Printf.sprintf "Choose CD-ROM device." in
      Arg.(value & opt string default_device & info ["d"; "device"] ~doc)

    let query_disc ~device ~verbose ~print_id ~print_freedb ~print_toc =
      if verbose then
        Printf.printf "querying %s ...\n" device;
      match Pml.Discid.get ~device () with
      | Result.Ok ids ->
         begin
           if not print_id && not print_freedb && not print_toc then
             Printf.printf "id = %s\nfreedb = %s\ntoc = %s\n" ids.id ids.freedb ids.toc
           else if print_id then
             print_endline ids.id
           else if print_freedb then
             print_endline ids.freedb
           else if print_toc then
             print_endline ids.toc
         end;
         0
      | Result.Error msg ->
         Printf.eprintf "error: %s!\n" msg;
         1

    let cmd =
      let open Cmd in
      make (info "disc" ~man) @@
        let+ device and+ verbose and+ print_id and+ print_freedb and+ print_toc in
        query_disc ~device ~verbose ~print_id ~print_freedb ~print_toc

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
        [ Query_Disc.cmd;
          Musicbrainz.cmd;
          Cachetest.cmd]

  end

let main () =
  Cmd.eval' Main.cmd

let () =
  if !Sys.interactive then
    ()
  else
    exit (main ())
