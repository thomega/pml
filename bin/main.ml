let () =
  match Pml.Discid.get () with
  | Result.Ok id -> Printf.printf "diskid = %s\n" id
  | Result.Error msg -> Printf.eprintf "error: %s!\n" msg

