type t =
  { title : string option;
    artists : Artist.Collection.t;
    media : Medium.t list;
    id : string }

let of_mb mb =
  let module R = Mb_release in
  let id = mb.R.id
  and title = mb.R.title
  and artists = Artist.of_credits mb.R.artist_credits
  and media = List.map Medium.of_mb mb.R.media in
  { id; title; artists; media }

