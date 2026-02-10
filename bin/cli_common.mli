(* cli_common.mli -- part of PML (Physical Media Library)

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

val default_cache : string

val root : string Cmdliner.Term.t

module type Common =
  sig
    val man_footer : Cmdliner.Manpage.block list
  end

module Common : Common

module type Unit_Result_Cmd =
  sig
    val cmd : (unit, string) result Cmdliner.Cmd.t
  end



