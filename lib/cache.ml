module type T =
  sig
    val init : root:string -> (unit, string) result
    val get : root:string -> string -> (string option, string) result
    val remove : root:string -> string -> (unit, string) result
    val set : root:string -> string -> string -> (unit, string) result
    val map : root:string -> string -> (string -> (string, string) result)-> (unit, string) result
  end

module type Table =
  sig
    val name : string
  end

module Make (Table : Table) : T =
  struct

    open Result.Syntax

    let is_directory name =
      if Sys.file_exists name then
        if Sys.is_directory name then
          Ok name
        else
          Error ("not a directory: " ^ name)
      else
        Error ("no such file or directory: " ^ name)
    
    let table ~root =
      let* root = is_directory root in
      is_directory (Filename.concat root Table.name)

    let filename ~root key =
      table ~root
      |> Result.map (fun path -> Filename.concat path key)

    let init ~root =
      let path = Filename.concat root Table.name in
      if Sys.is_directory path then
        Ok ()
      else if Sys.file_exists path then
        Error ("Cache().init: not a directory: " ^ path)
      else if Sys.is_directory root then
        try
          Ok (Sys.mkdir path 0o700)
        with
        | exn -> Error (Printexc.to_string exn)
      else if Sys.file_exists root then
        Error ("Cache().init: not a directory: " ^ root)
      else
        try
          Ok (Sys.mkdir root 0o700; Sys.mkdir path 0o700)
        with
        | exn -> Error (Printexc.to_string exn)
        
    let get ~root key =
      let* name = filename ~root key in
      if Sys.file_exists name then
        try
          Ok (Some (In_channel.with_open_text name In_channel.input_all))
        with
        | exn -> Error (Printexc.to_string exn)
      else
        Ok None

    let remove ~root key =
      let* name = filename ~root key in
      if Sys.file_exists name then
        try
          Ok (Sys.remove name)
        with
        | exn -> Error (Printexc.to_string exn)
      else
        Ok ()

    let set ~root key text =
      let* name = filename ~root key in
      try
        Ok (Out_channel.with_open_text name (fun oc -> Out_channel.output_string oc text))
      with
      | exn -> Error (Printexc.to_string exn)

    let map ~root key f =
      let* name = filename ~root key in
      if Sys.file_exists name then
        try
          let text = In_channel.with_open_text name In_channel.input_all in
          let* text' = f text in
          Ok (if text' <> text then
                Out_channel.with_open_text name (fun oc -> Out_channel.output_string oc text'))
        with
        | exn -> Error (Printexc.to_string exn)
      else
        Error (Printf.sprintf "entry '%s' not found in table '%s'" key Table.name)

  end

let%test_module _ =
  (module struct

     module C = Make (struct let name = "t" end)

     let%test _ =
       match C.init ~root:"r" with
       | Error _ -> false
       | Ok _ -> true

   end)

