open Ctypes

module Types = Types_generated

module Functions (F : Ctypes.FOREIGN) = struct
  open F

  let discid_new = foreign "discid_new" (void @-> returning void ptr)

end
