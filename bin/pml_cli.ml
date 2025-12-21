(* let version = "0.0.0" *)

open Cmdliner
(* open Cmdliner.Term.Syntax *)

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

    let query_disc _verbose id freedb toc =
      match Pml.Discid.get () with
      | Result.Ok ids ->
         begin
           if not id && not freedb && not toc then
             Printf.printf "id = %s\nfreedb = %s\ntoc = %s\n" ids.id ids.freedb ids.toc
           else if id then
             print_endline ids.id
           else if freedb then
             print_endline ids.freedb
           else if toc then
             print_endline ids.toc
         end;
         0
      | Result.Error msg ->
         Printf.eprintf "error: %s!\n" msg;
         1

    let term =
      let open Term in
      const query_disc
      $ verbose
      $ print_id
      $ print_freedb
      $ print_toc

    let cmd =
      Cmd.(make (info "disc" ~man) term)

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
        [ Query_Disc.cmd ]

  end

let main () =
  Cmd.eval' Main.cmd

let () =
  if !Sys.interactive then
    ()
  else
    exit (main ())
