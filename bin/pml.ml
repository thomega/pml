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

open Pml_lib
open Cli_common

open Cmdliner
open Cmdliner.Term.Syntax

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

module Medium : Unit_Result_Cmd =
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

  end

module Editor : Unit_Result_Cmd =
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
          ?no_originals ?no_recordings ~edits () =
      let open Result.Syntax in
      let* id = get_discid ?device ?discid () in
      let* disc = Taggable.of_discid ~root ?medium id in
      let* tagged = Tagged.Edits.apply_all edits (Tagged.of_mb disc) in
      Ok (Tagged.print ?no_artists ?factor_artists ?no_originals ?no_recordings tagged)

    let cmd =
      let open Cmd in
      let edits = Cli_edits.all in
      make (info "edit" ~man) @@
        let+ root and+ medium and+ discid and+ device
           and+ no_artists and+ factor_artists
           and+ no_originals and+ no_recordings and+ edits in
        f ~root ?medium ?discid ~device ~no_artists ~factor_artists
          ~no_originals ~no_recordings ~edits ()

  end

module Ripper : Unit_Result_Cmd =
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
                  String.concat ", " (List.map Rip.encoder_to_string Rip.encoders) ^
                    "]. The default is \"opus\"." in
      let enc_enum =
        List.map (fun enc -> (Rip.encoder_to_string enc, enc)) Rip.encoders in
      Arg.(value & opt_all (enum enc_enum) [Rip.Opus] & info ["e"; "encoder"] ~doc ~docv:"encoder")

    module ESet = Set.Make (struct type t = Rip.encoder let compare = Stdlib.compare end)

    let f ~root ?medium ?discid ?device ?directory ~dry ~verbose ~edits
          ~bitrate ~encoders () =
      let open Result.Syntax in
      let* id = get_discid ?device ?discid () in
      let* disc = Taggable.of_discid ~root ?medium id in
      let* tagged = Tagged.Edits.apply_all edits (Tagged.of_mb disc) in
      let encoders = ESet.of_list encoders |> ESet.elements in
      Rip.execute ~dry ~verbose ?directory ~bitrate encoders tagged

    let cmd =
      let open Cmd in
      let edits = Cli_edits.all in
      make (info "rip" ~man) @@
        let+ dry and+ verbose and+ directory
           and+ root and+ medium and+ discid and+ device
           and+ bitrate and+ encoders and+ edits in
        f ~root ~dry ~verbose ?directory ?medium ?discid ~device ~bitrate ~encoders ~edits ()

  end

module Disc : Unit_Result_Cmd =
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

end

module Version : Unit_Result_Cmd =
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
      Ok ()
    
    let cmd =
      let open Cmd in
      make (info "version" ~man) @@
        let+ license in
        version ~license

end

module Main : Unit_Result_Cmd =
  struct

    let _doc = "Physical Media Library"
    
    let man =
      [ `S Manpage.s_synopsis;
        `P "$(cmd) [$(i,OPTIONS)]";
        `S Manpage.s_description;
        `P "Query the CD-ROM and the MusicBrainz database.";
        `S Manpage.s_authors;
        `P "Thorsten Ohl <ohl@physik.uni-wuerzburg.de>" ] @ Common.man_footer

    let cmd =
      let open Cmd in
      group (info "pml" ~man)
        [ Disc.cmd;
          Medium.cmd;
          Editor.cmd;
          Ripper.cmd;
          Version.cmd]

  end

let main () =
  Cmd.eval_result Main.cmd

let () =
  if !Sys.interactive then
    ()
  else
    exit (main ())
