let () =
  match Pml.Discid.get () with
  | Result.Ok ids ->
     Printf.printf "id = %s\nfreedb = %s\ntoc = %s\n" ids.id ids.freedb ids.toc
  | Result.Error msg -> Printf.eprintf "error: %s!\n" msg

