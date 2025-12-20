open Ctypes

module Types = Types_generated

module Functions (F : Ctypes.FOREIGN) = struct
  open F

  let discid_new =
    foreign "discid_new"
      (void @-> returning (ptr void))

  let discid_read_sparse =
    foreign "discid_read_sparse"
      ((ptr void) @-> string_opt @-> int @-> returning int)

  let discid_get_id =
    foreign "discid_get_id"
      ((ptr void) @-> returning string)

end
