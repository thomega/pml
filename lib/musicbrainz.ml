let opt_list = function
  | Some l -> l
  | None -> []

module Release =
  struct
    type t =
      { id : string option;
        title : string option;
        artist_credit : Jsont.json list;
        media : Jsont.json list;
        unknown : Jsont.json }
    let make id title _packaging _release_group artist_credit media unknown =
      let artist_credit = opt_list artist_credit
      and media = opt_list media in
      { id; title; artist_credit; media; unknown }
    let jsont =
      Jsont.Object.map ~kind:"Release" make
      |> Jsont.Object.opt_mem "id" Jsont.string
      |> Jsont.Object.opt_mem "title" Jsont.string
      |> Jsont.Object.opt_mem "packaging" Jsont.ignore
      |> Jsont.Object.opt_mem "release-group" Jsont.ignore
      |> Jsont.Object.opt_mem "artist-credit" Jsont.(list json)
      |> Jsont.Object.opt_mem "media" Jsont.(list json)
      |> Jsont.Object.keep_unknown Jsont.json_mems
      |> Jsont.Object.finish
  end

let of_file name =
  let releases = Release.jsont in
  let text = In_channel.with_open_text name In_channel.input_all in
  Jsont_bytesrw.decode_string releases text
