module type T =
  sig
    val get : root:string -> string -> (string option, string) result
    val remove : root:string -> string -> (unit, string) result
    val set : root:string -> string -> string -> (unit, string) result
    val map : root:string -> (string -> (string, string) result) -> string-> (unit, string) result
  end

module type Table =
  sig
    val name : string
  end

module Make (Table : Table) : T =
  struct

    let is_directory name =
      if Sys.file_exists name then
        if Sys.is_directory name then
          Ok name
        else
          Error ("not a directory: " ^ name)
      else
        Error ("no such file or directory: " ^ name)
    
    let table ~root =
      match is_directory root with
      | Error _ as e -> e
      | Ok root -> is_directory (Filename.concat root Table.name)

    let filename ~root key =
      table ~root
      |>  Result.map (fun path -> Filename.concat path key)

    let get ~root key =
      match filename ~root key with
      | Error _ as e -> e
      | Ok name ->
         if Sys.file_exists name then
           try
             Ok (Some (In_channel.with_open_text name In_channel.input_all))
           with
           | exn -> Error (Printexc.to_string exn)
         else
           Ok None

    let remove ~root key =
      match filename ~root key with
      | Error _ as e -> e
      | Ok name ->
         if Sys.file_exists name then
           try
             Ok (Sys.remove name)
           with
           | exn -> Error (Printexc.to_string exn)
         else
           Ok ()

    let set ~root key text =
      match filename ~root key with
      | Error _ as e -> e
      | Ok name ->
         try
           Ok (Out_channel.with_open_text name (fun oc -> Out_channel.output_string oc text))
         with
         | exn -> Error (Printexc.to_string exn)

    let map ~root f key =
      match filename ~root key with
      | Error _ as e -> e
      | Ok name ->
         if Sys.file_exists name then
           try
             let text = In_channel.with_open_text name In_channel.input_all in
             match f text with
             | Error _ as e -> e
             | Ok text' ->
                Ok (if text' <> text then
                      Out_channel.with_open_text name (fun oc -> Out_channel.output_string oc text'))
           with
           | exn -> Error (Printexc.to_string exn)
         else
           Error (Printf.sprintf "entry '%s' not found in table '%s'" key Table.name)

  end
