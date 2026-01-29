let jsont =
  Jsont.Object.map Fun.id
  |> Jsont.Object.keep_unknown Jsont.json_mems ~enc:Fun.id
  |> Jsont.Object.finish

let compare_members ((name1, _), _) ((name2, _), _) =
  String.compare name1 name2

let sort_members members =
  List.sort compare_members members

let map_members f members =
  List.map (fun (name, json) -> (name, f json)) members

let rec sort_json = function
  | Jsont.Object (members, meta) ->
     Jsont.Object (sort_members (map_members sort_json members), meta)
  | Jsont.Array (members, meta) ->
     Jsont.Array (List.map sort_json members, meta)
  | atom -> atom

let of_file name =
  In_channel.with_open_text name In_channel.input_all
  |> Jsont_bytesrw.decode_string jsont

let print_json json =
  match Jsont_bytesrw.encode_string ~format:Jsont.Indent jsont (sort_json json) with
  | Ok text -> print_endline text
  | Error msg -> prerr_endline msg

let print_file name =
  match of_file name with
  | Ok json -> print_json json
  | Error msg -> prerr_endline msg

let normalize text =
  let open Result.Syntax in
  let* json = Jsont_bytesrw.decode_string jsont text in
  Jsont_bytesrw.encode_string ~format:Jsont.Indent jsont (sort_json json)

let indent pfx = pfx ^ "  "

let rec dump_schema_json pfx = function
  | Jsont.Object (mem_list, _meta) ->
     List.iter (dump_schema_mem pfx) mem_list
  | Jsont.Array ([], _meta) -> ()
  | Jsont.Array (json :: _ as json_list, _meta) ->
     Printf.printf "%s%d*\n" pfx (List.length json_list);
     dump_schema_json (indent pfx) json
  | _ -> ()

and dump_schema_mem pfx ((name, _meta), json) =
  Printf.printf "%s%s\n" pfx name;
  dump_schema_json (indent pfx) json

let dump_schema json =
  dump_schema_json "" json

let dump_schema json =
  Printexc.print dump_schema json

let dump_schema_file name =
  match of_file name with
  | Ok json -> dump_schema json
  | Error msg -> prerr_endline msg
