(* cli_ls.ml -- part of PML (Physical Media Library)

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

module Ls_artists : Unit_Result_Cmd =
  struct

    let man = [
        `S Manpage.s_description;
        `P "List the artists in the local cache, search for entries
            and trigger updates." ] @ Common.man_footer

    let f ~root () =
      let open Result.Syntax in
      let* artists = Cached.Artist.all_local ~root in
      let* artists =
        Result_list.map
          (fun (key, text) ->
            let* a = Jsont_bytesrw.decode_string Mb_artist.jsont text in
            let a = Artist.of_mb a in
            Ok (key, Ubase.from_utf8 a.Artist.sort_name, a.Artist.name))
          artists in
      let artists =
        List.sort (fun (_, s1, _) (_, s2, _) -> String.compare s1 s2) artists in
      let width =
        List.fold_left (fun acc (_, s, _) -> max acc (String.length s)) 0 artists in
      List.iter
        (fun (key, sort_name, name) ->
          Printf.printf "%s %-*s %s\n" key width sort_name name)
        artists;
      Ok ()

    let cmd =
      let open Cmd in
      let doc = "List the artists in the local cache." in
      make (info "artists" ~doc ~man) @@
        let+ root in
        f ~root ()

  end

let man = [
    `S Manpage.s_description;
    `P "List the contents of the local cache, search for entries
        and trigger updates." ] @ Common.man_footer

let cmd =
  let open Cmd in
  group (info "ls" ~man)
    [ Ls_artists.cmd ]
