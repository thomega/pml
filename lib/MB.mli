module JSON = Yojson.Basic
val parse_json : string -> JSON.t

val interpret_json : JSON.t -> unit

