let is_directory name =
  if Sys.file_exists name then
    if Sys.is_directory name then
      Ok name
    else
      Error ("not a directory: " ^ name)
  else
    Error ("no such file or directory: " ^ name)
    
let dir ~root ~subdir =
  match is_directory root with
  | Error _ as e -> e
  | Ok root -> is_directory (Filename.concat root subdir)

let filename ~root ~subdir tag =
  dir ~root ~subdir
  |>  Result.map (fun path -> Filename.concat path tag)

let lookup ~root ~subdir tag =
  match filename ~root ~subdir tag with
  | Error _ as e -> e
  | Ok name ->
     if Sys.file_exists name then
       try
         Ok (Some (In_channel.with_open_text name In_channel.input_all))
       with
       | exn -> Error (Printexc.to_string exn)
     else
       Ok None

let replace ~root ~subdir tag text =
  match filename ~root ~subdir tag with
  | Error _ as e -> e
  | Ok name ->
     try
       Ok (Out_channel.with_open_text name (fun oc -> Out_channel.output_string oc text))
     with
     | exn -> Error (Printexc.to_string exn)

let delete ~root ~subdir tag =
  match filename ~root ~subdir tag with
  | Error _ as e -> e
  | Ok name ->
     if Sys.file_exists name then
       try
         Ok (Sys.remove name)
       with
       | exn -> Error (Printexc.to_string exn)
     else
       Ok ()
