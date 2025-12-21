open Ctypes

module Types = Libdiscid_types

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

end
