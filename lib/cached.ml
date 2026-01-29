module type Table =
  sig

    val valid_key : string -> (string, string) result
    (** Check validity of the key, to avoid unnecessary
        lookups of invalid key. *)

    val query : Query.query
    (** What to ask Musicbrainz. *)

  end

(* A sequence of exactly 28 characters from the set [A-Za-z0-9._-]. *)

(* [Re.alnum] contains accented characters! *)

let alphanum =
  Re.(alt [rg 'A' 'Z'; rg 'a' 'z'; rg '0' '9'])

(* NB: '-' can appear only as padding at the end.  We must check
   the length of the strong as well. *)

let re_discid =
  Re.(seq [start; alt [alphanum; set "._"] |> rep1; set "-" |> rep; stop] |> compile)

let _is_discid s =
  String.length s = 28 && Re.execp re_discid s

(* NB: The DTD at http://musicbrainz.org/development/mmd/ is stricter:
   exactly one '-' at the end! *)

let re_discid =
  Re.(seq [start; repn (alt [alphanum; set "._"]) 27 (Some 27); set "-"; stop] |> compile)

let is_discid s =
  Re.execp re_discid s

let valid_discid discid =
  if is_discid discid then
    Ok discid
  else
    Error (Printf.sprintf "'%s' is not a valid discid" discid)

module Discid_table : Table =
  struct

    let valid_key = valid_discid

    let query = Query.{ table = "discid"; inc = [] }

  end

(* A MBID/UUID: 8-4-4-4-12 hex digits [0-9a-fA-F] *)

let hexrep n =
  Re.(repn xdigit n (Some n))

let re_uuid =
  Re.(seq [start;
           hexrep 8; set "-";
           hexrep 4; set "-";
           hexrep 4; set "-";
           hexrep 4; set "-";
           hexrep 12;
           stop]
      |> compile)

let is_uuid s =
  Re.execp re_uuid s

let valid_mbid mbid =
  if is_uuid mbid then
    Ok (String.lowercase_ascii mbid)
  else
    Error (Printf.sprintf "'%s' is not a valid MBID" mbid)

module Release_table : Table =
  struct

    let valid_key = valid_mbid

    let query =
      Query.{ table = "release";
              inc = ["artists"; "artist-credits"; "aliases";
                     "recordings"; "release-groups"; "discids";
                     "url-rels"; "labels"; ] }

  end

module Artist_table : Table =
  struct

    let valid_key = valid_mbid

    let query =
      Query.{ table = "artist";
              inc = ["aliases"] }

  end

module type T =
  sig
    val get : root:string -> string -> (string, string) result
    val local : root:string -> string -> (string option, string) result
    val remote : string -> (string, string) result
    val all_local : root:string -> ((string * string) list, string) result
    val url : string -> (string, string) result
    module M : Map.S with type key = string
    val map_of_ids : root:string -> (string -> ('a, string) result) ->
                     string list -> ('a M.t, string) result
    module Internal : Cache.T with type key = string and type value = string
  end

open Result.Syntax

module Make (Table : Table) : T =
  struct

    module C =
      Cache.Make
        (struct
          let name = Table.query.table
          type key = string
          let key_of_string = Table.valid_key
          let key_to_string = Table.valid_key
          type value = string
          let value_of_string = Result.ok
          let value_to_string = Result.ok
        end)

    (* This version deosn't check its argument.
       It can be used in [get] below,
       because the argument has been checked. *)
    let remote_unsafe key =
      let* text = Query.(exec musicbrainz Table.query key) in
      Mb_raw.normalize text

    let remote key =
      let* key = Table.valid_key key in
      remote_unsafe key

    let local = C.get

    let get ~root key =
      let* text = C.lookup ~root key remote_unsafe in
      match Mb_error.get_error_opt text with
      | Some msg -> Error (Printf.sprintf "get '%s' returns error object: %s" key msg)
      | None -> Ok text

    let url key =
      let* key = Table.valid_key key in
      Ok (Query.(url musicbrainz Table.query key))

    let all_local = C.to_alist

    module M = Map.Make (String)

    let map_of_ids ~root value_of_string keys =
      Result_list.fold_left
        (fun map key ->
          let* text = get ~root key in
          let* value = value_of_string text in
          Ok (M.add key value map))
        M.empty keys

    module Internal = C
  end

module Discid = Make (Discid_table)
module Release = Make (Release_table)
module Artist = Make (Artist_table)

let opt_list = function
  | Some l -> l
  | None -> []

module Release_Short =
  struct
    type t =
      { id : string }
    let make id =
      { id }
    let jsont =
      Jsont.Object.map ~kind:"Release_Short" make
      |> Jsont.Object.mem "id" Jsont.string
      |> Jsont.Object.finish
  end

module Discid_short =
  struct
    type t =
      { id : string;
        releases : Release_Short.t list }
    let make id releases =
      let releases = opt_list releases in
      { id; releases }
    let jsont =
      Jsont.Object.map ~kind:"Discid" make
      |> Jsont.Object.mem "id" Jsont.string
      |> Jsont.Object.opt_mem "releases" Jsont.(list Release_Short.jsont)
      |> Jsont.Object.finish

  end

let releases_of_discid ~root discid =
  let* json = Discid.get ~root discid in
  let* discid' = Jsont_bytesrw.decode_string Discid_short.jsont json in
  if discid'.Discid_short.id <> discid then
    Error (Printf.sprintf "inconsistent discid: '%s' <> '%s'" discid'.Discid_short.id discid)
  else
    match discid'.Discid_short.releases with
    | [] -> Error "No Releases"
    | releases -> Ok (List.map (fun r -> r.Release_Short.id) releases)

