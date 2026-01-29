type t =
  { name : string option;
    artist : Mb_artist.t option }

let make name artist =
  let name = Edit.blank_to_none name in
  { name; artist }

let jsont =
  Jsont.Object.map ~kind:"Artist_Credit" make
  |> Jsont.Object.opt_mem "name" Jsont.string
  |> Jsont.Object.opt_mem "artist" Mb_artist.jsont
  |> Jsont.Object.finish

let artist_id c =
  match c.artist with
  | None -> Sets.MBID.empty
  | Some artist -> Mb_artist.id artist
      
let update_artist map c =
  let open Result.Syntax in
  match c.artist with
  | Some artist ->
     let* artist = Mb_artist.update map artist in
     Ok { c with artist = Some artist }
  | None -> Ok c

let to_string c =
  match c.artist with
  | Some artist -> Mb_artist.to_string artist
  | None -> Option.value c.name ~default:"(anonymous)"
