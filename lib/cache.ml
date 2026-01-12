module type T =
  sig
    type key
    type value
    val init : root:string -> (unit, string) result
    val get : root:string -> key -> (value option, string) result
    val lookup : root:string -> key -> (key -> (value, string) result) -> (value, string) result
    val refresh : root:string -> key -> (key -> (value, string) result) -> (unit, string) result
    val remove : root:string -> key -> (unit, string) result
    val set : root:string -> key -> value -> (unit, string) result
    val map : root:string -> key -> (value -> (value, string) result)-> (unit, string) result
    val to_alist : root:string -> ((key * value) list, string) result
  end

module type Table =
  sig
    val name : string
    type key
    val key_of_string : string -> key
    val key_to_string : key -> string
    type value
    val value_of_string : string -> value
    val value_to_string : value -> string
  end

module Make (Table : Table) : T with type key = Table.key and type value = Table.value =
  struct

    type key = Table.key
    type value = Table.value

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
      let key = Table.key_to_string key in
      table ~root
      |> Result.map (fun path -> Filename.concat path key)

    let init ~root =
      let path = Filename.concat root Table.name in
      if Sys.file_exists root then
        if Sys.is_directory root then
          if Sys.file_exists path then
            if Sys.is_directory path then
              Ok ()
            else
              Error ("Cache().init: not a directory: " ^ path)
          else
            try
              Ok (Sys.mkdir path 0o700)
            with
            | exn -> Error (Printexc.to_string exn)
        else
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
          let value = In_channel.with_open_text name In_channel.input_all in
          let value = Table.value_of_string value in
          Ok (Some value)
        with
        | exn -> Error (Printexc.to_string exn)
      else
        Ok None

    let lookup ~root key source =
      let* name = filename ~root key in
      if Sys.file_exists name then
        try
          let value = In_channel.with_open_text name In_channel.input_all in
          let value = Table.value_of_string value in
          Ok value
        with
        | exn -> Error (Printexc.to_string exn)
      else
        let* value = source key in
        try
          let value' = Table.value_to_string value in
          Out_channel.with_open_text name (fun oc -> Out_channel.output_string oc value');
          Ok value
        with
        | exn -> Error (Printexc.to_string exn)

    let refresh ~root key source =
      let* name = filename ~root key in
      if Sys.file_exists name then
        try
          let value = In_channel.with_open_text name In_channel.input_all in
          let* value' = source key in
          let value = Table.value_of_string value in
          Ok (if value' <> value then
                let value' = Table.value_to_string value' in
                Out_channel.with_open_text name (fun oc -> Out_channel.output_string oc value'))
        with
        | exn -> Error (Printexc.to_string exn)
      else
        let* value = source key in
        try
          let value = Table.value_to_string value in
          Ok (Out_channel.with_open_text name (fun oc -> Out_channel.output_string oc value))
        with
        | exn -> Error (Printexc.to_string exn)
        

    let remove ~root key =
      let* name = filename ~root key in
      if Sys.file_exists name then
        try
          Ok (Sys.remove name)
        with
        | exn -> Error (Printexc.to_string exn)
      else
        Ok ()

    let set ~root key value =
      let value = Table.value_to_string value in
      let* name = filename ~root key in
      try
        Ok (Out_channel.with_open_text name (fun oc -> Out_channel.output_string oc value))
      with
      | exn -> Error (Printexc.to_string exn)

    let map ~root key f =
      let* name = filename ~root key in
      if Sys.file_exists name then
        try
          let value = Table.value_of_string (In_channel.with_open_text name In_channel.input_all) in
          let* value' = f value in
          Ok (if value' <> value then
                let value' = Table.value_to_string value' in
                Out_channel.with_open_text name (fun oc -> Out_channel.output_string oc value'))
        with
        | exn -> Error (Printexc.to_string exn)
      else
        let key = Table.key_to_string key in
        Error (Printf.sprintf "entry '%s' not found in table '%s'" key Table.name)

    let to_alist ~root =
      let* dir = table ~root in
      try
        Array.to_list (Sys.readdir dir)
        |> List.map
             (fun key ->
               (Table.key_of_string key,
                Table.value_of_string (In_channel.with_open_text (Filename.concat dir key) In_channel.input_all)))
        |> Result.ok
      with
      | exn -> Error (Printexc.to_string exn)
      
  end

let%test_module _ =
  (module struct

     open Result.Syntax

     module STable (N : sig val name : string end) = 
       struct
         let name = N.name
         type key = string
         let key_of_string = Fun.id
         let key_to_string = Fun.id
         type value = string
         let value_of_string = Fun.id
         let value_to_string = Fun.id
       end

     module C = Make (STable (struct let name = "t" end))

     let n = ref 0
     let fresh () =
       incr n;
       Printf.sprintf "cache-%d" !n

     let%test _ =
       let root = fresh () in
       C.init ~root |> Result.is_ok

     let%test _ =
       let root = fresh () in
       C.get ~root "a" |> Result.is_error

     let%test _ =
       let root = fresh () in
       match
         let* _ = C.init ~root in
         C.get ~root "a"
       with
       | Ok None -> true
       | _ -> false

     let%test _ =
       let root = fresh () in
       match
         let* _ = C.init ~root in
         let* _ = C.set ~root "a" "A" in
         C.get ~root "a"
       with
       | Ok (Some "A") -> true
       | _ -> false

     let%test _ =
       let root = fresh () in
       match
         let* _ = C.init ~root in
         let* _ = C.set ~root "a" "A" in
         let* _ = C.set ~root "a" "B" in
         C.get ~root "a"
       with
       | Ok (Some "B") -> true
       | _ -> false

     let%test _ =
       let root = fresh () in
       match
         let* _ = C.init ~root in
         let* _ = C.set ~root "a" "A" in
         let* _ = C.set ~root "b" "A" in
         C.get ~root "a"
       with
       | Ok (Some "A") -> true
       | _ -> false

     let%test _ =
       let root = fresh () in
       match
         let* _ = C.init ~root in
         let* _ = C.set ~root "a" "A" in
         let* _ = C.remove ~root "a" in
         C.get ~root "a"
       with
       | Ok None -> true
       | _ -> false

     let%test _ =
       let root = fresh () in
       match
         let* _ = C.init ~root in
         let* _ = C.set ~root "a" "A" in
         let* _ = C.map ~root "a" (fun s -> Ok (s ^ s)) in
         C.get ~root "a"
       with
       | Ok (Some "AA") -> true
       | _ -> false

     let%test _ =
       let root = fresh () in
       match
         let* _ = C.init ~root in
         let* _ = C.set ~root "a" "A" in
         let* _ = C.map ~root "a" (fun _ -> Error "foo") in
         C.get ~root "a"
       with
       | Error "foo" -> true
       | _ -> false

     let alists_equal l1 l2 =
       (List.sort compare l1) = (List.sort compare l2)

     let%test _ =
       let root = fresh () in
       match
         let* _ = C.init ~root in
         C.to_alist ~root
       with
       | Ok alist -> alists_equal alist []
       | _ -> false

     let%test _ =
       let root = fresh () in
       match
         let* _ = C.init ~root in
         let* _ = C.set ~root "a" "A" in
         C.to_alist ~root
       with
       | Ok alist -> alists_equal alist [("a", "A")]
       | _ -> false

     let%test _ =
       let root = fresh () in
       match
         let* _ = C.init ~root in
         let* _ = C.set ~root "a" "A" in
         let* _ = C.set ~root "b" "B" in
         C.to_alist ~root
       with
       | Ok alist -> alists_equal alist [("a", "A"); ("b", "B")]
       | _ -> false

     let%test _ =
       let root = fresh () in
       match
         let* _ = C.init ~root in
         let* _ = C.lookup ~root "a" (fun _ -> Ok "A") in
         C.to_alist ~root
       with
       | Ok alist -> alists_equal alist [("a", "A")]
       | _ -> false

     let%test _ =
       let root = fresh () in
       match
         let* _ = C.init ~root in
         let* _ = C.lookup ~root "a" (fun _ -> Ok "A") in
         let* _ = C.lookup ~root "a" (fun _ -> failwith "") in
         let* _ = C.lookup ~root "b" (fun _ -> Ok "B") in
         C.to_alist ~root
       with
       | Ok alist -> alists_equal alist [("a", "A"); ("b", "B")]
       | _ -> false

     let%test _ =
       let root = fresh () in
       match
         let* _ = C.init ~root in
         let* _ = C.lookup ~root "a" (fun _ -> Ok "A") in
         let* _ = C.lookup ~root "a" (fun _ -> Ok "X") in
         let* _ = C.lookup ~root "b" (fun _ -> Ok "B") in
         C.to_alist ~root
       with
       | Ok alist -> alists_equal alist [("a", "A"); ("b", "B")]
       | _ -> false

     let%test _ =
       let root = fresh () in
       match
         let* _ = C.init ~root in
         let* _ = C.lookup ~root "a" (fun _ -> Error "Z") in
         C.set ~root "a" "A"
       with
       | Error "Z" -> true
       | _ -> false

     let%test _ =
       let root = fresh () in
       match
         let* _ = C.init ~root in
         let* _ = C.set ~root "a" "A" in
         let* _ = C.lookup ~root "a" (fun _ -> Error "Z") in
         C.get ~root "a"
       with
       | Ok (Some "A") -> true
       | _ -> false

   end)
