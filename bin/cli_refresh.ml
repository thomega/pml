(* cli_refresh.ml -- part of PML (Physical Media Library)

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

module Refresh_artists : Unit_Result_Cmd =
  struct

    let man = [
        `S Manpage.s_description;
        `P "Refresh an artist in the local cache." ] @ Common.man_footer

    let artist =
      let doc = Printf.sprintf "The MBID of the artist to update." in
      Arg.(required & pos 0 (some string) None & info [] ~docv:"MBID" ~doc)

    let f ~root ~artist () =
      let open Result.Syntax in
      let* updated = Cached.Artist.refresh ~root artist in
      if updated then
        Printf.printf "artist %s updated\n" artist
      else
        Printf.printf "artist %s unchanged\n" artist;
      Ok ()

    let cmd =
      let open Cmd in
      let doc = "Update an artist in the local cache." in
      make (info "artist" ~doc ~man) @@
        let+ root and+ artist in
        f ~root ~artist ()

  end

module Refresh_releases : Unit_Result_Cmd =
  struct

    let man = [
        `S Manpage.s_description;
        `P "Update a release in the local cache." ] @ Common.man_footer

    let release =
      let doc = Printf.sprintf "The MBID of the release to update." in
      Arg.(required & pos 0 (some string) None & info [] ~docv:"MBID" ~doc)

    let f ~root ~release () =
      let open Result.Syntax in
      let* updated = Cached.Release.refresh ~root release in
      if updated then
        Printf.printf "release %s updated\n" release
      else
        Printf.printf "release %s unchanged\n" release;
      Ok ()

    let cmd =
      let open Cmd in
      let doc = "Update a release in the local cache." in
      make (info "release" ~doc ~man) @@
        let+ root and+ release in
        f ~root ~release ()

  end

module Refresh_discids : Unit_Result_Cmd =
  struct

    let man = [
        `S Manpage.s_description;
        `P "Update a discid in the local cache." ] @ Common.man_footer

    let discid =
      let doc = Printf.sprintf "The MBID of the discid to update." in
      Arg.(required & pos 0 (some string) None & info [] ~docv:"MBID" ~doc)

    let f ~root ~discid () =
      let open Result.Syntax in
      let* updated = Cached.Discid.refresh ~root discid in
      if updated then
        Printf.printf "discid %s updated\n" discid
      else
        Printf.printf "discid %s unchanged\n" discid;
      Ok ()

    let cmd =
      let open Cmd in
      let doc = "Update a discid in the local cache." in
      make (info "discid" ~doc ~man) @@
        let+ root and+ discid in
        f ~root ~discid ()

  end

let man = [
    `S Manpage.s_description;
    `P "Refresh the contents of the local cache." ] @ Common.man_footer

let cmd =
  let open Cmd in
  group (info "refresh" ~man)
    [ Refresh_artists.cmd;
      Refresh_releases.cmd;
      Refresh_discids.cmd ]
