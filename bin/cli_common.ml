(* cli_common.ml -- part of PML (Physical Media Library)

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

let default_cache =
  match Sys.getenv_opt "HOME" with
  | Some home -> Filename.concat home ".local/share/pml/cache"
  | None -> "pml-cache"

open Cmdliner

let root =
  let doc = Printf.sprintf "Path to the root directory of the local cache."
  and env = Cmd.Env.info "MUSICBRAINZ_CACHE" in
  Arg.(value & opt dirpath default_cache & info ["cache"] ~docv:"path" ~doc ~env)

module type Common =
  sig
    val man_footer : Manpage.block list
  end

module Common : Common =
  struct 

    let man_footer =
      [ `S Manpage.s_files;
        `I ("$(b," ^ default_cache ^ ")",
            "Directory containg cached JSON responses from MusicBrainz.");
        `S Manpage.s_authors;
        `P "Thorsten Ohl <ohl@physik.uni-wuerzburg.de>.";
        `S Manpage.s_bugs;
        `P "Report bugs to <ohl@physik.uni-wuerzburg.de>.";
        `P "Needs more testing on different discs.
            The command line interface is still messy." ]

  end

module type Unit_Result_Cmd =
  sig
    val cmd : (unit, string) result Cmd.t
  end
