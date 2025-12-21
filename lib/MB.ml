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

let interpret_json json =
  let rec interpret_json' pfx json =
    match json with
    | `Assoc assoc ->
       List.iter
         (fun (name, subtree) ->
           Printf.printf "%s%s = \n" pfx name;
           interpret_json' (pfx ^ "  ") subtree ) assoc
    | `List jsons ->
       Printf.printf "%s[\n" pfx;
       List.iter (interpret_json' (pfx ^ "  ")) jsons;
       Printf.printf "%s]\n" pfx;
    | `Null -> Printf.printf "%sNULL\n" pfx
    | `Bool bool -> Printf.printf "%s%b\n" pfx bool
    | `Int n -> Printf.printf "%s%d\n" pfx n
    | `Float x -> Printf.printf "%s%g\n" pfx x
    | `String s -> Printf.printf "%s%s\n" pfx s in
  interpret_json' "" json;
  Printf.printf "keys:\n";
  List.iter print_endline (JSON.Util.keys json);
  Printf.printf "id:\n";
  interpret_json' "- " (JSON.Util.member "id" json);
  Printf.printf "releases:\n";
  interpret_json' "- " (JSON.Util.member "releases" json)

