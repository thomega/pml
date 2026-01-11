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

    open Result
    open Result.Syntax

    let is_directory name =
      if Sys.file_exists name then
        if Sys.is_directory name then
          ok name
        else
          error ("not a directory: " ^ name)
      else
        error ("no such file or directory: " ^ name)
    
    let table ~root =
      let* root = is_directory root in
      is_directory (Filename.concat root Table.name)

    let filename ~root key =
      table ~root
      |>  Result.map (fun path -> Filename.concat path key)

    let get ~root key =
      let* name = filename ~root key in
      if Sys.file_exists name then
        try
          ok (Some (In_channel.with_open_text name In_channel.input_all))
        with
        | exn -> error (Printexc.to_string exn)
      else
        ok None

    let remove ~root key =
      let* name = filename ~root key in
      if Sys.file_exists name then
        try
          ok (Sys.remove name)
        with
        | exn -> error (Printexc.to_string exn)
      else
        ok ()

    let set ~root key text =
      let* name = filename ~root key in
      try
        ok (Out_channel.with_open_text name (fun oc -> Out_channel.output_string oc text))
      with
      | exn -> error (Printexc.to_string exn)

    let map ~root f key =
      let* name = filename ~root key in
      if Sys.file_exists name then
        try
          let text = In_channel.with_open_text name In_channel.input_all in
          let* text' = f text in
          ok (if text' <> text then
                Out_channel.with_open_text name (fun oc -> Out_channel.output_string oc text'))
        with
        | exn -> error (Printexc.to_string exn)
      else
        error (Printf.sprintf "entry '%s' not found in table '%s'" key Table.name)

  end
