type t =
  { title : string option;
    tracks : Track.t list;
    id : string }

let of_mb mb =
  let module M = Mb_medium in
  let id = mb.M.id
  and title = mb.M.title
  and tracks = List.map Track.of_mb mb.M.tracks in
  { id; title; tracks }
