let () =
  begin match Pml.Discid.get () with
  | Result.Ok id -> Printf.printf "discid = %s\n" id
  | Result.Error msg -> Printf.eprintf "error: %s!\n" msg
  end;
  match Pml.Discid.get_freedb () with
  | Result.Ok id -> Printf.printf "freedb_id = %s\n" id
  | Result.Error msg -> Printf.eprintf "error: %s!\n" msg

