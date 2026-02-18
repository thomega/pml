(* cli_edits.ml -- part of PML (Physical Media Library)

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
open Cmdliner
open Cmdliner.Term.Syntax

let title =
  let doc = Printf.sprintf "Overwrite derived title." in
  Arg.(value & opt (some string) None & info ["t"; "title"] ~docv:"title" ~doc)

let perl_s =
  let parser = Perl.S.of_string in
  let pp ppf sub = Format.pp_print_string ppf (Perl.S.to_string sub) in
  Cmdliner.Arg.Conv.make ~docv:"/regexp/substitution/flags" ~parser ~pp ()

let perl_m =
  let parser = Perl.M.of_string in
  let pp ppf pat = Format.pp_print_string ppf (Perl.M.to_string pat) in
  Cmdliner.Arg.Conv.make ~docv:"/regexp/flags" ~parser ~pp ()

let perl_s_ranged =
  let parser = Perl.S.ranged_of_string in
  let pp ppf sub = Perl.S.ranged_to_string sub |> Format.pp_print_string ppf in
  Cmdliner.Arg.Conv.make ~docv:"range/regexp/substitution/flags" ~parser ~pp ()

let perl_m_ranged =
  let parser = Perl.M.ranged_of_string in
  let pp ppf pat = Perl.M.ranged_to_string pat |> Format.pp_print_string ppf in
  Cmdliner.Arg.Conv.make ~docv:"range/regexp/flags" ~parser ~pp ()

let string_ranged =
  let open Result.Syntax in
  let parser s =
    let* ranges, name = Edit.ranged_of_string Result.ok s in
    let l = String.length name in
    if l <= 0 then
      Error (Printf.sprintf {|empty name in "%s"|} s)
    else if name.[0] = ':' then
      Ok (ranges, String.sub name 1 (pred l))
    else
      Ok (ranges, name)
  and pp ppf (ranges, name) =
    Edit.ranges_to_string ranges ^ ":" ^ name
    |> Format.pp_print_string ppf in
  Cmdliner.Arg.Conv.make ~docv:"range:name" ~parser ~pp ()

let regexp_flags_doc =
  "The flags accepted are 'i' for case insensitive,
   'g' for repeated matching, and 'x' for extended syntax."

let ranged_doc =
  "The optional ranges are specified as a comma separated list of integers
   and intervals, e.g. $(b,1-3,5).  Absent ranges denote all tracks, of course.
   The tracks are numbered from 1, $(b,after) track selection."

let edit_doc =
  String.concat " "
    ["Note that the matching delimiters '/' can be replaced by any other character,
      but they can not be escaped by '\\\\' in the expressions.";
     regexp_flags_doc]

let ranged_edit_doc =
  String.concat " "
    ["Note that the matching delimiters '/' can be replaced by any other character,
      except digits, but they can not be escaped by '\\\\' in the expressions.";
     regexp_flags_doc;
     ranged_doc]

let edit_track_titles =
  let doc =
    String.concat " "
      ["Edit the original track titles with a pair of perl regular expression
        and substitution strings $(b,before) the extraction of a common prefix.";
       ranged_edit_doc;
       "Repeated arguments are applied in sequence."] in
  Arg.(value & opt_all perl_s_ranged [] & info ["edit_track_titles"] ~doc)

(*
  let default =
    match Perl.S.of_string "/:.*$//" with
    | Ok expr -> expr
    | _ -> assert false in
 *)

let edit_prefix =
  let doc =
    String.concat " "
      ["Edit the common prefix with a pair of perl regular expression and substitution
        string.  E.g. $(b,--edit_prefix '/:.*$//') will chop off everything after a colon.
        This is helpful, if all track portions start with the same letter, for example
        if the movements of a classical piece are enumerated my roman numerals
        and there are fewer that five movements.";
       edit_doc] in
  Arg.(value & opt_all perl_s [] & info ["edit_prefix"] ~doc)

let edit_title =
  let doc =
    String.concat " "
      ["Edit the title after all other edits. This does $(b,not) affect the extraction
        of the common prefix, but allows to normalize directory names.
        E.g. if there are titles containing single and double digit numbers,
        $(b,--edit_title '/ (\\\\d\\) / 0\\$1 /') will add a
        leading zero to single digits for simpler sorting. ";
       edit_doc] in
  Arg.(value & opt_all perl_s [] & info ["edit_title"] ~doc)

let recording_titles =
  let doc = "Choose the recording titles as track titles." in
  Arg.(value & flag & info ["R"; "recording"] ~doc)

let medium_title =
  let doc = "Choose the medium title." in
  Arg.(value & flag & info ["m"; "medium"] ~doc)

let release_title =
  let doc = "Choose the release title." in
  Arg.(value & flag & info ["r"; "release"] ~doc)

let delete_artists =
  let doc =
    String.concat " "
      ["Remove artists with name matching a $(b,perl)-style regular expression
        from the tracks specified.";
       ranged_doc] in
  Arg.(value & opt_all perl_m_ranged [] & info ["delete_artists"] ~docv:"ranges/regexp/flags" ~doc)

let delete_artists_sort =
  let doc =
    String.concat " "
      ["Remove artists with sort_name matching a $(b,perl)-style regular expression
        from the tracks specified.";
       ranged_doc] in
  Arg.(value & opt_all perl_m_ranged [] & info ["delete_artists_sort"] ~docv:"ranges/regexp/flags" ~doc)

let edit_artists =
  let doc =
    String.concat " "
      ["Edit the names of artists with a pair of perl regular expression and substitution strings.";
       ranged_edit_doc;
       "Repeated arguments are applied in sequence."] in
  Arg.(value & opt_all perl_s_ranged [] & info ["edit_artists"] ~doc)

let add_artist =
  let doc =
    String.concat " "
      ["Add artists by name.";
       ranged_doc;
       "Repeated arguments are applied in sequence."] in
  Arg.(value & opt_all string_ranged [] & info ["add_artist"] ~doc)

let composer =
  let doc = "Overwrite derived composer (top billing)." in
  Arg.(value & opt (some string) None & info ["c"; "composer"] ~docv:"name" ~doc)

let performer =
  let doc = "Overwrite derived performer (top billing)." in
  Arg.(value & opt (some string) None & info ["p"; "performer"] ~docv:"name" ~doc)

let composer_pattern =
  let doc =
    String.concat " "
      ["Overwrite derived composer (top billing) by the one matching the regular expression.";
       edit_doc] in
  Arg.(value & opt (some perl_m) None & info ["C"; "Composer"] ~docv:"/regexp/flags" ~doc)

let performer_pattern =
  let doc =
    String.concat " "
      ["Overwrite derived performer (top billing) by the one matching the regular expression.";
       edit_doc] in
  Arg.(value & opt (some perl_m) None & info ["P"; "Performer"] ~docv:"/regexp/flags" ~doc)

let offset =
  let doc = "Apply an offset to the track numbers." in
  Arg.(value & opt int Tagged.(default_trackset.offset) & info ["o"; "offset"] ~docv:"n" ~doc)

let first =
  let doc = "First track to select (counting from 1, $(b,before) applying offset)." in
  Arg.(value & opt int Tagged.(default_trackset.first) & info ["f"; "first"] ~docv:"n" ~doc)

let last =
  let doc = "Last track to select (counting from 1, $(b,before) applying offset)." in
  Arg.(value & opt (some int) Tagged.(default_trackset.last) & info ["l"; "last"] ~docv:"n" ~doc)

let width =
  let doc = "The width of the printed track number, including leading zeros." in
  Arg.(value & opt int Tagged.(default_trackset.width) & info ["w"; "width"] ~docv:"n" ~doc)

let single =
  let doc = "The piece consists of a single movement." in
  Arg.(value & flag & info ["s"; "single"] ~doc)

let unitary =
  let doc = "The composer and performer are a unit, don't append the latter to the
             directory name." in
  Arg.(value & flag & info ["u"; "unitary"] ~doc)

let trackset =
  let+ offset and+ first and+ last and+ width and+ single in
  let ts = Tagged.{ offset; first; last; width; single } in
  if ts = Tagged.default_trackset then
    None
  else
    Some ts

let all =
  let+ title and+ edit_prefix and+ edit_title
     and+ recording_titles and+ release_title and+ medium_title
     and+ delete_artists and+ delete_artists_sort and+ edit_artists and+ add_artist
     and+ composer and+ composer_pattern and+ performer and+ performer_pattern
     and+ unitary and+ trackset and+ edit_track_titles in
  Tagged.Edits.{ title; edit_prefix; edit_title; recording_titles; release_title; medium_title;
                 delete_artists; delete_artists_sort; edit_artists; add_artist; edit_track_titles;
                 composer; composer_pattern; performer; performer_pattern; unitary; trackset }

