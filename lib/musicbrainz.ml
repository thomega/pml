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
      { id : string option }
    let make id =
      { id }
    let jsont =
      Jsont.Object.map ~kind:"Release_Short" make
      |> Jsont.Object.opt_mem "id" Jsont.string
      |> Jsont.Object.finish
  end

module Disc_Id =
  struct
    type t =
      { id : string option;
        releases : Release_Short.t list }
    let make id releases =
      let releases = opt_list releases in
      { id; releases }
    let jsont =
      Jsont.Object.map ~kind:"Diskid" make
      |> Jsont.Object.opt_mem "id" Jsont.string
      |> Jsont.Object.opt_mem "releases" Jsont.(list Release_Short.jsont)
      |> Jsont.Object.finish

  end

let discid_of_file name =
  let text = In_channel.with_open_text name In_channel.input_all in
  Jsont_bytesrw.decode_string Disc_Id.jsont text

let release_ids disc_id =
  List.fold_right
    (fun r ids ->
      match r.Release_Short.id with
      | Some id -> id :: ids
      | None -> ids)
    disc_id.Disc_Id.releases [] 

module Artist_Credit =
  struct
    type t =
      { name : string option;
        artist : Jsont.json option;
        unknown : Jsont.json }
    let make name artist unknown =
      { name; artist; unknown }
    let jsont =
      Jsont.Object.map ~kind:"Artist_Credit" make
      |> Jsont.Object.opt_mem "name" Jsont.string
      |> Jsont.Object.opt_mem "artist" Jsont.json
      |> Jsont.Object.keep_unknown Jsont.json_mems
      |> Jsont.Object.finish
  end

module Media =
  struct
    type t =
      { id : string option;
        unknown : Jsont.json }
    let make id unknown =
      { id; unknown }
    let jsont =
      Jsont.Object.map ~kind:"Media" make
      |> Jsont.Object.opt_mem "id" Jsont.string
      |> Jsont.Object.keep_unknown Jsont.json_mems
      |> Jsont.Object.finish
  end

module Release =
  struct
    type t =
      { id : string option;
        title : string option;
        artist_credit : Artist_Credit.t list;
        media : Media.t list }
    let make id title artist_credit media =
      let artist_credit = opt_list artist_credit
      and media = opt_list media in
      { id; title; artist_credit; media }
    let jsont =
      Jsont.Object.map ~kind:"Release" make
      |> Jsont.Object.opt_mem "id" Jsont.string
      |> Jsont.Object.opt_mem "title" Jsont.string
      |> Jsont.Object.opt_mem "artist-credit" Jsont.(list Artist_Credit.jsont)
      |> Jsont.Object.opt_mem "media" Jsont.(list Media.jsont)
      |> Jsont.Object.finish
  end

let release_of_file name =
  let text = In_channel.with_open_text name In_channel.input_all in
  Jsont_bytesrw.decode_string Release.jsont text



