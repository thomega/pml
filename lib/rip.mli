(* rip.mli -- part of PML (Physical Media Library)

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

(** Ripping and tagging. *)

type encoder = Opus | Vorbis | Flac | Mp3
val encoder_to_string : encoder -> string
val encoders : encoder list

type extra_args =
  { cdparanoia : string list;
    opus : string list;
    vorbis : string list;
    flac : string list;
    mp3 : string list }

val default_extra_args : extra_args

val execute : ?dry:bool -> ?verbose:bool -> ?directory:string -> extra_args ->
              bitrate:int -> encoder list -> Tagged.t -> (unit, string) result
(** Execute the external programs for ripping, encoding and tagging. *)
