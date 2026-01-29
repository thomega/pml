(* query.ml -- part of PML (Physical Media Library)

   Copyright (C) 2026 by Thorsten Ohl <ohl@physik.uni-wuerzburg.de>

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>. *)

let client_version = Version.version
let client_name = Version.long_name
let contact = Version.email

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
    timeout = Some 60 }

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

let fmt_error url msg =
  Printf.sprintf "curl %s failed: %s" url msg

(* We can live with an explicit state here, because
   [libcurl] is stateful anyway. *)
let last_curl = ref 0.0
let curl_interval = 2.0

let curl ?timeout ~user_agent url =
  let wait = !last_curl -. Unix.time () +. curl_interval in
  if wait > 0. then
    begin
      Printf.printf "sleeping for %g seconds to pacify the MusicBrainz servers... " wait;
      flush stdout;
      Unix.sleepf wait;
      Printf.printf "done.\n";
      flush stdout
    end;
  let result = Buffer.create 16384
  and error_response = ref "" in
  Curl.global_init Curl.CURLINIT_GLOBALALL;
  try
    let curl = Curl.init () in
    Curl.set_url curl url;
    Curl.set_followlocation curl true;
    Option.iter (Curl.set_timeout curl) timeout;
    Curl.set_useragent curl user_agent;
    Curl.set_errorbuffer curl error_response;
    Curl.set_writefunction curl (write_to result);
    Curl.perform curl;
    Curl.cleanup curl;
    Curl.global_cleanup ();
    last_curl := Unix.time ();
    Ok (Buffer.contents result)
  with
  | Curl.CurlException (curlcode, _code, _msg) ->
     begin
       Curl.global_cleanup ();
       last_curl := Unix.time ();
       match !error_response with
       | "" -> Error (fmt_error url (Curl.strerror curlcode))
       | s -> Error (fmt_error url s)
     end

let exec api query key =
  curl ?timeout:api.timeout ~user_agent:api.user_agent (url musicbrainz query key)
