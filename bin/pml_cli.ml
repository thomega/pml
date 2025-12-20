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

module type Unit_Cmd =
  sig
    val cmd : unit Cmd.t
  end

module Disc : Unit_Cmd =
  struct

    let man = [
        `S Manpage.s_description;
        `P "Query the disc in the drive." ] @ Common.man_footer

    let term =
      let open Term in
      const
        (fun () ->
          match Pml.Discid.get () with
          | Result.Ok ids ->
             Printf.printf "id = %s\nfreedb = %s\ntoc = %s\n" ids.id ids.freedb ids.toc
          | Result.Error msg -> Printf.eprintf "error: %s!\n" msg)
      $ const ()

    let cmd =
      Cmd.v (Cmd.info "disc" ~man) term

end

module Main : Unit_Cmd =
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
      Cmd.group
        (Cmd.info "pml" ~man)
        [ Disc.cmd ]

  end

let main () =
  Cmd.eval Main.cmd

let () =
  if !Sys.interactive then
    ()
  else
    exit (main ())
