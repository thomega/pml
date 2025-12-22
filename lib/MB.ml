module JSON = Yojson.Basic

let separator = String.make 72 '='

let parse_json s =
  try
    JSON.from_string s
  with
  | Yojson.Json_error msg ->
     Printf.eprintf
       "Response is not valid JSON:\n%s\n%s\n%s\n%s\n"
       msg separator s separator;
     flush stderr;
     failwith "invalid JSON"

let dump_json json =
  let rec dump_json' pfx json =
    match json with
    | `Assoc assoc ->
       List.iter
         (fun (name, subtree) ->
           Printf.printf "%s%s = \n" pfx name;
           dump_json' (pfx ^ "  ") subtree ) assoc
    | `List jsons ->
       Printf.printf "%s[\n" pfx;
       List.iter (dump_json' (pfx ^ "  ")) jsons;
       Printf.printf "%s]\n" pfx;
    | `Null -> Printf.printf "%sNULL\n" pfx
    | `Bool bool -> Printf.printf "%s%b\n" pfx bool
    | `Int n -> Printf.printf "%s%d\n" pfx n
    | `Float x -> Printf.printf "%s%g\n" pfx x
    | `String s -> Printf.printf "%s%s\n" pfx s in
  dump_json' "" json

let print_keys path json =
  let path = String.concat "/" (List.rev path) in
  Printf.printf "keys /%s:\n" path;
  JSON.Util.keys json |> List.iter (Printf.printf "  %s\n")
  
let interpret_json json =
  print_keys [] json;
  let id = JSON.Util.member "id" json |> JSON.Util.to_string in
  Printf.printf "id = %s\n" id;
  let releases = JSON.Util.member "releases" json in
  List.iter (print_keys ["releases"]) (JSON.Util.to_list releases);
  let media = JSON.Util.member "media" (JSON.Util.to_list releases |> List.hd) in
  List.iter (print_keys ["media"; "releases"]) (JSON.Util.to_list media)
    
let interpret_json json =
  Printexc.print interpret_json json

let rec dump_schema' pfx json =
  let keys = JSON.Util.keys json in
  List.iter
    (fun key ->
      Printf.printf "%s%s\n" pfx key;
      match JSON.Util.member key json with
      | `Assoc _ as a -> dump_schema' (pfx ^ ">  ") a
      | `List l -> List.iter (dump_schema' (pfx ^ "+  ")) l
      | _ -> ())
    keys

let dump_schema json =
  dump_schema' "" json

let dump_schema json =
  Printexc.print dump_schema json
