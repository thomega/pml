(** Cache and process JSON responses of the MusicBrainz database. *)

(** Process the JSON files returned by MusicBrainz uninterpreted.
    NB: except [normalize], all functions are for debugging only. *)

val normalize : string -> (string, string) result
(** Decode and encode the JSON in the string by [Jsont] to obtain
    a canonical representation. *)

val of_file : string -> (Jsont.json, string) result
(** Decode a JSON stored in a file. *)

val print_file : string -> unit
(** Decode a JSON stored in a file, encode it and write it to
    standard output. *)

val dump_schema_file : string -> unit
(** Decode a JSON stored in a file and write the structure to standard
    output. *)
