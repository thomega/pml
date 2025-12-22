let version = "0.0.0"
let _user_agent = "Physical Media Library/" ^ version ^ " ( ohl@physik.uni-wuerzburg.de )"

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

module Musicbrainz : Exit_Cmd =
  struct

    let man = [
        `S Manpage.s_description;
        `P "Experimental Musicbrainz JSON parsing." ] @ Common.man_footer

    let file =
      let doc = Printf.sprintf "JSON file." in
      Arg.(value & opt (some string) None & info ["f"; "file"] ~doc)

    let dump =
      let doc = "Dump the whole tree without syntax elements." in
      Arg.(value & flag & info ["d"; "dump"] ~doc)

    let schema =
      let doc = "Dump the whole tree without contents." in
      Arg.(value & flag & info ["s"; "schema"] ~doc)

    let parse_json ?file ~dump ~schema () =
      match file with
      | None -> 0
      | Some name ->
         let raw = In_channel.with_open_text name In_channel.input_all in
         try
           let json = Pml.MB.parse_json raw in
           if schema then
             Pml.MB.dump_schema json
           else if dump then
             Pml.MB.dump_json json
           else
             Pml.MB.interpret_json json;
           0
         with
         | _ -> 1

    let cmd =
      let open Cmd in
      make (info "musicbrainz" ~man) @@
        let+ file and+ dump and+ schema in
        parse_json ?file ~dump ~schema ()

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
          Musicbrainz.cmd]

  end

let main () =
  Cmd.eval' Main.cmd

let () =
  if !Sys.interactive then
    ()
  else
    exit (main ())
