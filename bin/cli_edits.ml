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

let edit_doc =
  "Note that the '/' can be replaced by any other
   character, but it can not be escaped by '\\\\'
   in the expressions.
   The only flags accepted are 'i' for case insensitive
   and 'g' for repeated matching.
   Repeated arguments are applied in sequence."

let edit_prefix =
  let doc = Printf.sprintf "Edit the common prefix with a pair
                            of perl regular expression and substitution
                            string.  E.g. $(b,--edit_prefix '/:.*$//') will
                            chop off everything after a colon.  This
                            is helpful, if all track portions start with
                            the same letter, for example if the movements
                            of a classical piece are enumerated my roman
                            numerals and there are fewer that five movements. " ^ edit_doc in
  Arg.(value & opt_all perl_s [] & info ["edit_prefix"] ~doc)

let edit_title =
  let doc = Printf.sprintf "Edit the title after all other edits. This does
                            $(b,not) affect the extraction of the common
                            prefix, but allows to normalize directory names.
                            E.g. if there are titles containing single and
                            double digit numbers,
                            $(b,--edit_title '/ (\\\\d\\) / 0\\$1 /') will add a
                            leading zero to single digits for simpler sorting. " ^ edit_doc in
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

let all =
  let+ title and+ edit_prefix and+ edit_title
     and+ recording_titles and+ release_title and+ medium_title
     and+ composer and+ composer_prefix and+ performer and+ performer_prefix
     and+ trackset in
  Tagged.Edits.{ title; edit_prefix; edit_title; recording_titles; release_title; medium_title;
                 composer; composer_prefix; performer; performer_prefix; trackset }

