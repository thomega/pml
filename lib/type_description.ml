(* open Ctypes *)

module Types (F : Ctypes.TYPE) = struct
  open F

  let discid_version_num = constant "DISCID_VERSION_NUM" int

end
