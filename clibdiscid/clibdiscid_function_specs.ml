(* clibdiscid_function_specs.ml -- part of PML (Physical Media Library)

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

open Ctypes

module Types = Clibdiscid_types

module Functions (F : Ctypes.FOREIGN) = struct
  open F

  let default_device = foreign "discid_get_default_device" (void @-> returning string)
  let alloc = foreign "discid_new" (void @-> returning (ptr void))
  let free = foreign "discid_free" ((ptr void) @-> returning void)
  let read_sparse = foreign "discid_read_sparse" ((ptr void) @-> string_opt @-> int @-> returning int)
  let get_error_msg = foreign "discid_get_error_msg" ((ptr void) @-> returning string)
  let get_id = foreign "discid_get_id" ((ptr void) @-> returning string)
  let get_freedb_id = foreign "discid_get_freedb_id" ((ptr void) @-> returning string)
  let get_toc_string = foreign "discid_get_toc_string" ((ptr void) @-> returning string)
  let get_submission_url = foreign "discid_get_submission_url" ((ptr void) @-> returning string)

end
