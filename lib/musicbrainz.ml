let version = "0.0.1"
let user_agent = "Physical Media Library/" ^ version ^ " ( ohl@physik.uni-wuerzburg.de )"

type api =
  { ssl : bool;
    host : string;
    api : string;
    user_agent : string option;
    timeout : int option }

let musicbrainz =
  { ssl = true;
    host = "musicbrainz.org";
    api = "ws/2";
    user_agent = Some user_agent;
    timeout = Some 10 }

type query =
  { query : string;
    inc : string list }

let query_to_string query key =
  match query.inc with
  | [] -> query.query ^ "/" ^ key ^ "?fmt=json"
  | inc -> query.query ^ "/" ^ key ^ "?fmt=json&inc=" ^ String.concat "+" inc

let url_of_query options query key =
  let protocol =
    if options.ssl then
      "https"
    else
      "http" in
  protocol ^ "://" ^ options.host ^ "/" ^ options.api ^ "/" ^ query_to_string query key

let write_to buffer data =
  Buffer.add_string buffer data;
  String.length data

let do_curl options query key =
  let result = Buffer.create 16384
  and error_response = ref "" in
  Curl.global_init Curl.CURLINIT_GLOBALALL;
  try
    let curl = Curl.init () in
    Curl.set_url curl (url_of_query musicbrainz query key);
    begin match options.timeout with
    | None -> ()
    | Some t -> Curl.set_timeout curl t
    end;
    begin match options.user_agent with
    | None -> ()
    | Some a -> Curl.set_useragent curl a
    end;
    Curl.set_errorbuffer curl error_response;
    Curl.set_writefunction curl (write_to result);
    Curl.perform curl;
    Curl.cleanup curl;
    Curl.global_cleanup ();
    Ok (Buffer.contents result)
  with
  | Curl.CurlException (curlcode, _code, _msg) ->
     Curl.global_cleanup ();
     match !error_response with
     | "" -> Error (Curl.strerror curlcode)
     | s -> Error s

let query_discid =
  { query = "discid";
    inc = [] }

let query_release =
  { query = "release";
    inc = ["artists"; "artist-credits";
           "recordings"; "release-groups"; "discids";
           "url-rels";"labels"; ] }

let get_discid discid =
  do_curl musicbrainz query_discid discid

let get_release release =
  do_curl musicbrainz query_release release

let url_discid discid =
  url_of_query musicbrainz query_discid discid

let url_release release =
  url_of_query musicbrainz query_release release

let read_discid_cache _discid =
  None

let get_discid_cached discid =
  match read_discid_cache discid with
  | Some s -> Ok s
  | None -> get_discid discid

module type Raw =
  sig
    val of_file : string -> (Jsont.json, string) result
    val dump_schema_file : string -> unit
  end

module Raw : Raw =
  struct

    let jsont =
      Jsont.Object.map Fun.id
      |> Jsont.Object.keep_unknown Jsont.json_mems
      |> Jsont.Object.finish

    let of_file name =
      let text = In_channel.with_open_text name In_channel.input_all in
      Jsont_bytesrw.decode_string jsont text

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

module Disc_Id =
  struct
    type t =
      { id : string;
        releases : Release_Short.t list }
    let make id releases =
      let releases = opt_list releases in
      { id; releases }
    let jsont =
      Jsont.Object.map ~kind:"Diskid" make
      |> Jsont.Object.mem "id" Jsont.string
      |> Jsont.Object.opt_mem "releases" Jsont.(list Release_Short.jsont)
      |> Jsont.Object.finish

  end

let discid_of_file name =
  let text = In_channel.with_open_text name In_channel.input_all in
  Jsont_bytesrw.decode_string Disc_Id.jsont text

let release_ids disc_id =
  List.map (fun r -> r.Release_Short.id) disc_id.Disc_Id.releases

module Artist =
  struct
    type t =
      { id : string (** While this is optional in the DTD, it should be there anyway. *);
        name : string option;
        sort_name : string option;
        artist_type : string option;
        disambiguation : string option;
        unknown : Jsont.json }
    let make id name sort_name artist_type disambiguation unknown =
      { id; name; sort_name; artist_type; disambiguation; unknown }
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
        unknown : Jsont.json }
    let make name artist unknown =
      { name; artist; unknown }
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
        unknown : Jsont.json }
    let make id title artist_credit unknown =
      let artist_credit = opt_list artist_credit in
      { id; title; artist_credit; unknown }
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
        unknown : Jsont.json }
    let make id position title artist_credit recording unknown =
      let artist_credit = opt_list artist_credit in
      { id; position; title; artist_credit; recording; unknown }
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
        unknown : Jsont.json }
    let make id unknown =
      { id; unknown }
    let jsont =
      Jsont.Object.map ~kind:"Disc" make
      |> Jsont.Object.mem "id" Jsont.string
      |> Jsont.Object.keep_unknown Jsont.json_mems
      |> Jsont.Object.finish
  end

module Medium =
  struct
    type t =
      { id : string (** While this is optional in the DTD, it should be there anyway. *);
        position : int option;
        title : string option;
        discs : Disc.t list;
        tracks : Track.t list;
        unknown : Jsont.json }
    let make id position title discs tracks unknown =
      let discs = opt_list discs
      and tracks = opt_list tracks in
      { id; position; title; discs; tracks; unknown }
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
      { id : string (** While this is optional in the DTD, it should be there anyway. *);
        title : string option;
        artist_credit : Artist_Credit.t list;
        media : Medium.t list }
    let make id title artist_credit media =
      let artist_credit = opt_list artist_credit
      and media = opt_list media in
      { id; title; artist_credit; media }
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

let media_of_file discid name =
  release_of_file name
  |> Result.map (fun release -> List.filter (contains_discid discid) release.Release.media)

     



