open Result.Syntax

module type Raw =
  sig
    val normalize : string -> (string, string) result
    val of_file : string -> (Jsont.json, string) result
    val print_file : string -> unit
    val dump_schema_file : string -> unit
  end

module Raw : Raw =
  struct

    let jsont =
      Jsont.Object.map Fun.id
      |> Jsont.Object.keep_unknown Jsont.json_mems ~enc:Fun.id
      |> Jsont.Object.finish

    let of_file name =
      In_channel.with_open_text name In_channel.input_all
      |> Jsont_bytesrw.decode_string jsont

    let print_json json =
      match Jsont_bytesrw.encode_string ~format:Jsont.Indent jsont json with
      | Ok text -> print_endline text
      | Error msg -> prerr_endline msg

    let print_file name =
      match of_file name with
      | Ok json -> print_json json
      | Error msg -> prerr_endline msg

    let sort_members members =
      List.sort (fun ((name1, _), _) ((name2, _), _) -> String.compare name1 name2) members

    let map_members f members =
      List.map (fun (name, json) -> (name, f json)) members

    let rec sort_json = function
      | Jsont.Object (members, meta) ->
         Jsont.Object (sort_members (map_members sort_json members), meta)
      | atom -> atom

    let normalize text =
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

  end

module type Table =
  sig

    val valid_key : string -> (string, string) result
    (** Check validity of the key, to avoid unnecessary
        lookups of invalid key. *)

    val query : Query.query
    (** What to ask Musicbrainz. *)

  end

module Discid_table : Table =
  struct

    (* A sequence of exactly 28 characters from the set [A-Za-z0-9._-]. *)

    (* [Re.alnum] contains accented characters! *)

    let alphanum =
      Re.(alt [rg 'A' 'Z'; rg 'a' 'z'; rg '0' '9'])

    (* NB: '-' can appear only as padding at the end.  We must check
       the length of the strong as well. *)

    let re_discid =
      Re.(seq [start; alt [alphanum; set "._"] |> rep1; set "-" |> rep; stop] |> compile)

    let is_discid s =
      String.length s = 28 && Re.execp re_discid s

    let valid_key discid =
      if is_discid discid then
        Ok discid
      else
        Error (Printf.sprintf "'%s' is not a valid discid" discid)

    let query = Query.{ table = "discid"; inc = [] }

  end

module Release_table : Table =
  struct

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

    let valid_key mbid =
      if is_uuid mbid then
        Ok (String.lowercase_ascii mbid)
      else
        Error (Printf.sprintf "'%s' is not a valid MBID" mbid)

    let query =
      Query.{ table = "release";
              inc = ["artists"; "artist-credits"; "recordings";
                     "release-groups"; "discids";
                     "url-rels"; "labels"; ] }

  end

module type Cached_table =
  sig
    val get : root:string -> string -> (string, string) result
    val local : root:string -> string -> (string option, string) result
    val remote : string -> (string, string) result
    val all_local : root:string -> ((string * string) list, string) result
    val url : string -> (string, string) result
    module Internal : Cache.T with type key = string and type value = string
  end

module Cached_table (Table : Table) : Cached_table =
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
      Raw.normalize text

    let remote key =
      let* key = Table.valid_key key in
        remote_unsafe key

    let local = C.get

    let get ~root key =
      C.lookup ~root key remote_unsafe

    let url key =
      let* key = Table.valid_key key in
      Ok (Query.(url musicbrainz Table.query key))

    let all_local = C.to_alist

    module Internal = C
  end

module Discid_cached = Cached_table (Discid_table)
module Release_cached = Cached_table (Release_table)

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

module Discid =
  struct
    type t =
      { _id : string;
        releases : Release_Short.t list }
    let make _id releases =
      let releases = opt_list releases in
      { _id; releases }
    let jsont =
      Jsont.Object.map ~kind:"Diskid" make
      |> Jsont.Object.mem "id" Jsont.string
      |> Jsont.Object.opt_mem "releases" Jsont.(list Release_Short.jsont)
      |> Jsont.Object.finish

  end

module Artist =
  struct
    type t =
      { id : string (** While this is optional in the DTD, it should be there anyway. *);
        name : string option;
        sort_name : string option;
        artist_type : string option;
        disambiguation : string option;
        ignored : Jsont.json }
    let make id name sort_name artist_type disambiguation ignored =
      { id; name; sort_name; artist_type; disambiguation; ignored }
    let jsont =
      Jsont.Object.map ~kind:"Artist" make
      |> Jsont.Object.mem "id" Jsont.string
      |> Jsont.Object.opt_mem "name" Jsont.string
      |> Jsont.Object.opt_mem "sort-name" Jsont.string
      |> Jsont.Object.opt_mem "type" Jsont.string
      |> Jsont.Object.opt_mem "disambiguation" Jsont.string
      |> Jsont.Object.keep_unknown Jsont.json_mems
      |> Jsont.Object.finish
  end

module Artist_Credit =
  struct
    type t =
      { name : string option;
        artist : Artist.t option;
        ignored : Jsont.json }
    let make name artist ignored =
      { name; artist; ignored }
    let jsont =
      Jsont.Object.map ~kind:"Artist_Credit" make
      |> Jsont.Object.opt_mem "name" Jsont.string
      |> Jsont.Object.opt_mem "artist" Artist.jsont
      |> Jsont.Object.keep_unknown Jsont.json_mems
      |> Jsont.Object.finish
  end

module Recording =
  struct
    type t =
      { id : string (** While this is optional in the DTD, it should be there anyway. *);
        title : string option;
        artist_credit : Artist_Credit.t list;
        ignored : Jsont.json }
    let make id title artist_credit ignored =
      let artist_credit = opt_list artist_credit in
      { id; title; artist_credit; ignored }
    let jsont =
      Jsont.Object.map ~kind:"Recording" make
      |> Jsont.Object.mem "id" Jsont.string
      |> Jsont.Object.opt_mem "title" Jsont.string
      |> Jsont.Object.opt_mem "artist-credit" Jsont.(list Artist_Credit.jsont)
      |> Jsont.Object.keep_unknown Jsont.json_mems
      |> Jsont.Object.finish
  end

module Track =
  struct
    type t =
      { id : string (** While this is optional in the DTD, it should be there anyway. *);
        position : int option;
        title : string option;
        artist_credit : Artist_Credit.t list;
        recording : Recording.t option;
        ignored : Jsont.json }
    let make id position title artist_credit recording ignored =
      let artist_credit = opt_list artist_credit in
      { id; position; title; artist_credit; recording; ignored }
    let jsont =
      Jsont.Object.map ~kind:"Track" make
      |> Jsont.Object.mem "id" Jsont.string
      |> Jsont.Object.opt_mem "position" Jsont.int
      |> Jsont.Object.opt_mem "title" Jsont.string
      |> Jsont.Object.opt_mem "artist-credit" Jsont.(list Artist_Credit.jsont)
      |> Jsont.Object.opt_mem "recording" Recording.jsont
      |> Jsont.Object.keep_unknown Jsont.json_mems
      |> Jsont.Object.finish
  end

module Disc =
  struct
    type t =
      { id : string (** While this is optional in the DTD, it should be there anyway. *);
        _ignored : Jsont.json }
    let make id _ignored =
      { id; _ignored }
    let jsont =
      Jsont.Object.map ~kind:"Disc" make
      |> Jsont.Object.mem "id" Jsont.string
      |> Jsont.Object.keep_unknown Jsont.json_mems
      |> Jsont.Object.finish
  end

module Medium =
  struct
    type t =
      { _id : string (** While this is optional in the DTD, it should be there anyway. *);
        _position : int option;
        _title : string option;
        discs : Disc.t list;
        _tracks : Track.t list;
        _ignored : Jsont.json }
    let make _id _position _title discs tracks _ignored =
      let discs = opt_list discs
      and _tracks = opt_list tracks in
      { _id; _position; _title; discs; _tracks; _ignored }
    let jsont =
      Jsont.Object.map ~kind:"Medium" make
      |> Jsont.Object.mem "id" Jsont.string
      |> Jsont.Object.opt_mem "position" Jsont.int
      |> Jsont.Object.opt_mem "title" Jsont.string
      |> Jsont.Object.opt_mem "discs" Jsont.(list Disc.jsont)
      |> Jsont.Object.opt_mem "tracks" Jsont.(list Track.jsont)
      |> Jsont.Object.keep_unknown Jsont.json_mems
      |> Jsont.Object.finish
  end

module Release =
  struct
    type t =
      { _id : string (** While this is optional in the DTD, it should be there anyway. *);
        _title : string option;
        _artist_credit : Artist_Credit.t list;
        media : Medium.t list }
    let make _id _title artist_credit media =
      let _artist_credit = opt_list artist_credit
      and media = opt_list media in
      { _id; _title; _artist_credit; media }
    let jsont =
      Jsont.Object.map ~kind:"Release" make
      |> Jsont.Object.mem "id" Jsont.string
      |> Jsont.Object.opt_mem "title" Jsont.string
      |> Jsont.Object.opt_mem "artist-credit" Jsont.(list Artist_Credit.jsont)
      |> Jsont.Object.opt_mem "media" Jsont.(list Medium.jsont)
      |> Jsont.Object.finish
  end

let release_of_file name =
  let text = In_channel.with_open_text name In_channel.input_all in
  Jsont_bytesrw.decode_string Release.jsont text

let contains_discid discid medium =
  List.exists (fun disc -> discid = disc.Disc.id) medium.Medium.discs

let _media_of_file discid name =
  release_of_file name
  |> Result.map (fun release -> List.filter (contains_discid discid) release.Release.media)

let releases_of_discid ~root discid =
  let* json = Discid_cached.get ~root discid in
  let* discid = Jsont_bytesrw.decode_string Discid.jsont json in
  match discid.Discid.releases with
  | [] -> Error "no releases"
  | releases -> Ok (List.map (fun r -> r.Release_Short.id) releases)



