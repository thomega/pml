(* cache.ml -- part of PML (Physical Media Library)

   Copyright (C) 2026 by Thorsten Ohl <ohl@physik.uni-wuerzburg.de>

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>. *)

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
    val key_of_string : string -> (key, string) result
    val key_to_string : key -> (string, string) result
    type value
    val value_of_string : string -> (value, string) result
    val value_to_string : value -> (string, string) result
  end

module Make (Table : Table) : T with type key = Table.key and type value = Table.value =
  struct

    type key = Table.key
    type value = Table.value

    open Table
    open Result.Syntax

    module IC = In_channel
    module OC = Out_channel
    
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
      is_directory (Filename.concat root name)

    let filename ~root key =
      let* key = key_to_string key in
      table ~root
      |> Result.map (fun path -> Filename.concat path key)

    let init ~root =
      let path = Filename.concat root name in
      if Sys.file_exists root then
        if Sys.is_directory root then
          if Sys.file_exists path then
            if Sys.is_directory path then
              Ok ()
            else
              Error ("Cache.init: not a directory: " ^ path)
          else
            try
              Ok (Sys.mkdir path 0o700)
            with
            | exn -> Error (Printexc.to_string exn)
        else
          Error ("Cache.init: not a directory: " ^ root)
      else
        try
          Ok (Sys.mkdir root 0o700; Sys.mkdir path 0o700)
        with
        | exn -> Error (Printexc.to_string exn)
        
    let get ~root key =
      let* name = filename ~root key in
      if Sys.file_exists name then
        try
          let value = IC.with_open_text name IC.input_all in
          let* value = value_of_string value in
          Ok (Some value)
        with
        | exn -> Error (Printexc.to_string exn)
      else
        Ok None

    let lookup ~root key direct =
      let* name = filename ~root key in
      if Sys.file_exists name then
        try
          let value = IC.with_open_text name IC.input_all in
          let* value = value_of_string value in
          Ok value
        with
        | exn -> Error (Printexc.to_string exn)
      else
        let* value = direct key in
        let* value' = value_to_string value in
        try
          OC.with_open_text name (fun oc -> OC.output_string oc value');
          Ok value
        with
        | exn -> Error (Printexc.to_string exn)

    let refresh ~root key direct =
      let* name = filename ~root key in
      if Sys.file_exists name then
        try
          let value = IC.with_open_text name IC.input_all in
          let* value = value_of_string value
          and* value' = direct key in
          if value' <> value then
            let* value' = value_to_string value' in
            Ok (OC.with_open_text name (fun oc -> OC.output_string oc value'))
          else
            Ok ()
        with
        | exn -> Error (Printexc.to_string exn)
      else
        let* value = direct key in
        let* value = value_to_string value in
        try
          Ok (OC.with_open_text name (fun oc -> OC.output_string oc value))
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
      let* name = filename ~root key
      and* value = value_to_string value in
      try
        Ok (OC.with_open_text name (fun oc -> OC.output_string oc value))
      with
      | exn -> Error (Printexc.to_string exn)

    let map ~root key f =
      let* name = filename ~root key in
      if Sys.file_exists name then
        try
          let* value = value_of_string (IC.with_open_text name IC.input_all) in
          let* value' = f value in
          if value' <> value then
            let* value' = value_to_string value' in
            Ok (OC.with_open_text name (fun oc -> OC.output_string oc value'))
          else
            Ok ()
        with
        | exn -> Error (Printexc.to_string exn)
      else
        let* key = key_to_string key in
        Error (Printf.sprintf "entry '%s' not found in table '%s'" key name)

    let to_alist ~root =
      let* dir = table ~root in
      try
        Array.to_list (Sys.readdir dir)
        |> List.fold_left
             (fun acc key ->
               let value = IC.with_open_text (Filename.concat dir key) IC.input_all in
               let* acc = acc
               and* key = key_of_string key
               and* value = value_of_string value in
               Ok ((key, value) :: acc))
             (Ok [])
      with
      | exn -> Error (Printexc.to_string exn)
      
  end

let%test_module _ =
  (module struct

     open Result.Syntax

     module STable (N : sig val name : string end) = 
       struct
         let name = N.name

         type key = int (* only positive! *)

         let key_of_string s =
           match int_of_string_opt s with
           | None -> Error ("not an integer: " ^ s)
           | Some i ->
              if i < 0 then
                Error ("negative: " ^ s)
              else
                Ok i

         let key_to_string i =
           let s =string_of_int i in
           if i < 0 then
             Error ("negative: " ^ s)
           else
             Ok s

         type value = string
         let value_of_string = Result.ok
         let value_to_string = Result.ok
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
       C.get ~root 1 |> Result.is_error

     let%test _ =
       let root = fresh () in
       match
         let* () = C.init ~root in
         C.get ~root 1
       with
       | Ok None -> true
       | _ -> false

     let%test _ =
       let root = fresh () in
       match
         let* () = C.init ~root in
         C.set ~root (-1) "A"
       with
       | Error "negative: -1" -> true
       | _ -> false

     let%test _ =
       let root = fresh () in
       match
         let* () = C.init ~root in
         C.get ~root (-1)
       with
       | Error "negative: -1" -> true
       | _ -> false

     let%test _ =
       let root = fresh () in
       match
         let* () = C.init ~root in
         let* () = C.set ~root 1 "A" in
         C.get ~root 1
       with
       | Ok (Some "A") -> true
       | _ -> false

     let%test _ =
       let root = fresh () in
       match
         let* () = C.init ~root in
         let* () = C.set ~root 1 "A" in
         let* () = C.set ~root 1 "B" in
         C.get ~root 1
       with
       | Ok (Some "B") -> true
       | _ -> false

     let%test _ =
       let root = fresh () in
       match
         let* () = C.init ~root in
         let* () = C.set ~root 1 "A" in
         let* () = C.set ~root 2 "A" in
         C.get ~root 1
       with
       | Ok (Some "A") -> true
       | _ -> false

     let%test _ =
       let root = fresh () in
       match
         let* () = C.init ~root in
         let* () = C.set ~root 1 "A" in
         let* () = C.remove ~root 1 in
         C.get ~root 1
       with
       | Ok None -> true
       | _ -> false

     let%test _ =
       let root = fresh () in
       match
         let* () = C.init ~root in
         let* () = C.set ~root 1 "A" in
         let* () = C.map ~root 1 (fun s -> Ok (s ^ s)) in
         C.get ~root 1
       with
       | Ok (Some "AA") -> true
       | _ -> false

     let%test _ =
       let root = fresh () in
       match
         let* () = C.init ~root in
         let* () = C.set ~root 1 "A" in
         let* () = C.map ~root 1 (fun _ -> Error "foo") in
         C.get ~root 1
       with
       | Error "foo" -> true
       | _ -> false

     let%test _ =
       let root = fresh () in
       match
         let* () = C.init ~root in
         let* () = C.set ~root 1 "A" in
         C.lookup ~root 1 (fun _ -> Ok "B")
       with
       | Ok "A" -> true
       | _ -> false

     let alists_equal l1 l2 =
       (List.sort compare l1) = (List.sort compare l2)

     let%test _ =
       let root = fresh () in
       match
         let* () = C.init ~root in
         C.to_alist ~root
       with
       | Ok alist -> alists_equal alist []
       | _ -> false

     let%test _ =
       let root = fresh () in
       match
         let* () = C.init ~root in
         let* () = C.set ~root 1 "A" in
         C.to_alist ~root
       with
       | Ok alist -> alists_equal alist [(1, "A")]
       | _ -> false

     let%test _ =
       let root = fresh () in
       match
         let* () = C.init ~root in
         let* () = C.set ~root 1 "A" in
         let* () = C.set ~root 2 "B" in
         C.to_alist ~root
       with
       | Ok alist -> alists_equal alist [(1, "A"); (2, "B")]
       | _ -> false

     let%test _ =
       let root = fresh () in
       match
         let* () = C.init ~root in
         let* _ = C.lookup ~root 1 (fun _ -> Ok "A") in
         C.to_alist ~root
       with
       | Ok alist -> alists_equal alist [(1, "A")]
       | _ -> false

     let%test _ =
       let root = fresh () in
       match
         let* () = C.init ~root in
         let* _ = C.lookup ~root 1 (fun _ -> Ok "A") in
         let* _ = C.lookup ~root 1 (fun _ -> failwith "") in
         let* _ = C.lookup ~root 2 (fun _ -> Ok "B") in
         C.to_alist ~root
       with
       | Ok alist -> alists_equal alist [(1, "A"); (2, "B")]
       | _ -> false

     let%test _ =
       let root = fresh () in
       match
         let* () = C.init ~root in
         let* _ = C.lookup ~root 1 (fun _ -> Ok "A") in
         let* _ = C.lookup ~root 1 (fun _ -> Ok "X") in
         let* _ = C.lookup ~root 2 (fun _ -> Ok "B") in
         C.to_alist ~root
       with
       | Ok alist -> alists_equal alist [(1, "A"); (2, "B")]
       | _ -> false

     let%test _ =
       let root = fresh () in
       match
         let* () = C.init ~root in
         let* _ = C.lookup ~root 1 (fun _ -> Error "Z") in
         C.set ~root 1 "A"
       with
       | Error "Z" -> true
       | _ -> false

     let%test _ =
       let root = fresh () in
       match
         let* () = C.init ~root in
         let* () = C.set ~root 1 "A" in
         let* _ = C.lookup ~root 1 (fun _ -> Error "Z") in
         C.get ~root 1
       with
       | Ok (Some "A") -> true
       | _ -> false

   end)
