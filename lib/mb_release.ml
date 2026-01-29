type t =
  { id : string (** While this is optional in the DTD, it should be there anyway. *);
    title : string option;
    artist_credits : Mb_artist_credit.t list;
    media : Mb_medium.t list }

let make id title artist_credits media =
  let title = Edit.blank_to_none title
  and artist_credits = Option.value ~default:[] artist_credits
  and media = Option.value ~default:[] media in
  { id; title; artist_credits; media }

let jsont =
  Jsont.Object.map ~kind:"Release" make
  |> Jsont.Object.mem "id" Jsont.string
  |> Jsont.Object.opt_mem "title" Jsont.string
  |> Jsont.Object.opt_mem "artist-credit" Jsont.(list Mb_artist_credit.jsont)
  |> Jsont.Object.opt_mem "media" Jsont.(list Mb_medium.jsont)
  |> Jsont.Object.finish

let artist_ids r =
  Sets.MBID.union
    (List.map Mb_artist_credit.artist_id r.artist_credits |> Sets.mbid_union)
    (List.map Mb_medium.artist_ids r.media |> Sets.mbid_union)

let update_artists map r =
  let open Result.Syntax in
  let* artist_credits = Result_list.map (Mb_artist_credit.update_artist map) r.artist_credits
  and* media = Result_list.map (Mb_medium.update_artists map) r.media in
  Ok { r with artist_credits; media }
