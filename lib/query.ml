let client_version = "0.0.1"
let client_name = "Physical Media Library"
let contact = "ohl@physik.uni-wuerzburg.de"

type api =
  { ssl : bool;
    host : string;
    api : string;
    user_agent : string;
    timeout : int option }

let musicbrainz =
  { ssl = true;
    host = "musicbrainz.org";
    api = "ws/2";
    user_agent = client_name ^ "/" ^ client_version ^ " ( " ^ contact ^ " )";
    timeout = Some 10 }

type query =
  { table : string;
    inc : string list }

let query_to_string query key =
  match query.inc with
  | [] -> query.table ^ "/" ^ key ^ "?fmt=json"
  | inc -> query.table ^ "/" ^ key ^ "?fmt=json&inc=" ^ String.concat "+" inc

let url api query key =
  let protocol =
    if api.ssl then
      "https"
    else
      "http" in
  protocol ^ "://" ^ api.host ^ "/" ^ api.api ^ "/" ^ query_to_string query key

let write_to buffer data =
  Buffer.add_string buffer data;
  String.length data

let exec api query key =
  let result = Buffer.create 16384
  and error_response = ref "" in
  Curl.global_init Curl.CURLINIT_GLOBALALL;
  try
    let curl = Curl.init () in
    Curl.set_url curl (url musicbrainz query key);
    Curl.set_followlocation curl true;
    Option.iter (Curl.set_timeout curl) api.timeout;
    Curl.set_useragent curl api.user_agent;
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

