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

module Ls_releases : Unit_Result_Cmd =
  struct

    let man = [
        `S Manpage.s_description;
        `P "List the releases in the local cache, search for entries
            and trigger updates." ] @ Common.man_footer

    let f ~root () =
      let open Result.Syntax in
      let* releases = Cached.Release.all_local ~root in
      let* releases =
        Result_list.map
          (fun (key, text) ->
            let* a = Jsont_bytesrw.decode_string Mb_release.jsont text in
            let a = Release.of_mb a in
            Ok (key, Option.value ~default:"???" a.Release.title |> Ubase.from_utf8 ))
          releases in
      let releases =
        List.sort (fun (_, s1) (_, s2) -> String.compare s1 s2) releases in
      List.iter
        (fun (key, title) ->
          Printf.printf "%s %s\n" key title)
        releases;
      Ok ()

    let cmd =
      let open Cmd in
      let doc = "List the releases in the local cache." in
      make (info "releases" ~doc ~man) @@
        let+ root in
        f ~root ()

  end

module Ls_discids : Unit_Result_Cmd =
  struct

    let man = [
        `S Manpage.s_description;
        `P "List the releases in the local cache, search for entries
            and trigger updates." ] @ Common.man_footer

    let _f ~root () =
      let open Result.Syntax in
      let* discids = Cached.Discid.all_local ~root in
      let* discids =
        List.map fst discids
        |> List.sort String.compare
        |> Result_list.map
             (fun discid ->
               let* releases = Cached.releases_of_discid ~root discid in
               Ok (discid, List.sort String.compare releases)) in
      List.iter
        (fun (discid, releases) ->
          Printf.printf "%s %s\n" discid (String.concat " " releases))
        discids;
      Ok ()

    let f ~root () =
      let open Result.Syntax in
      let* discids = Cached.Discid.all_local ~root in
      List.map fst discids
      |> List.iter
           (fun discid ->
             match Taggable.of_discid_sans_lifespans ~root discid with
             | Error msg ->
                begin match Cached.releases_of_discid ~root discid with
                | Error msg ->
                   Printf.printf "%s error: %s\n" discid msg
                | Ok releases ->
                   Printf.printf "%s -> %s\n%s\n" discid (String.concat " " releases) msg
                end
             | Ok t ->
                Printf.printf
                  "%s '%s' '%s'\n"
                  t.Taggable.discid
                  (Option.value ~default:"???" t.Taggable.release.Mb_release.title)
                  (Option.value ~default:"" t.Taggable.medium.Mb_medium.title));
      Ok ()

    let cmd =
      let open Cmd in
      let doc = "List the discids in the local cache." in
      make (info "discids" ~doc ~man) @@
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
    [ Ls_artists.cmd;
      Ls_releases.cmd;
      Ls_discids.cmd ]
