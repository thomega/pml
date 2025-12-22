module JSON = Yojson.Basic
val parse_json : string -> JSON.t

val dump_json : JSON.t -> unit
val interpret_json : JSON.t -> unit

