let get ?device () =
  let open Discid_C.Functions in
  let handle = discid_new () in
  match discid_read_sparse handle device 0 with
  | 0 -> Result.Error "discid_read_sparse failed!"
  | _ ->
     let id = discid_get_id handle in
     Result.Ok id

