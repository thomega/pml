let get ?device () =
  ignore device;
  let handle = Discid_C.Functions.discid_new () in
  match Discid_C.Functions.discid_read_sparse handle None 0 with
  | 0 -> Result.Error "discid_read_sparse failed!"
  | _ ->
     let id = Discid_C.Functions.discid_get_id handle in
     Result.Ok id

