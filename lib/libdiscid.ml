(* libdiscid.ml -- part of PML (Physical Media Library)

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

type t =
  { id : string;
    freedb : string;
    toc : string;
    submission_url : string }

module type Raw =
  sig
    type disc
    val default_device : unit -> string
    val alloc : unit -> disc
    val free : disc -> unit
    val read_sparse : disc -> string option -> int -> int
    val get_error_msg : disc -> string
    val get_id : disc -> string
    val get_freedb_id : disc -> string
    val get_toc_string : disc -> string
    val get_submission_url : disc -> string
  end
(** The same low level functions as in the module Clibdiscid.Functions that
    is produced by Ctypes and dune.  But we make the disc type opaque. *)

module Raw : Raw =
  struct
    module L = Clibdiscid.Functions
    type disc = unit Ctypes_static.ptr
    let default_device = L.default_device
    let alloc = L.alloc
    let free = L.free
    let read_sparse = L.read_sparse
    let get_error_msg = L.get_error_msg
    let get_id = L.get_id
    let get_freedb_id = L.get_freedb_id
    let get_toc_string = L.get_toc_string
    let get_submission_url = L.get_submission_url
  end

let get ?device () =
  let open Raw in
  let disc = alloc () in
  match read_sparse disc device 0 with
  | 0 ->
     free disc;
     Result.Error ("libdiscid::read_sparse failed: " ^ get_error_msg disc)
  | _ ->
     let ids =
       { id = get_id disc;
         freedb = get_freedb_id disc;
         toc = String.map (fun c -> if c = ' ' then '+' else c) (get_toc_string disc);
         submission_url = get_submission_url disc } in
     free disc;
     Result.Ok ids

let default_device = Raw.default_device
