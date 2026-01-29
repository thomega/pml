type t =
  { number : int;  (** Overall position of the track in the whole work, counting from 1. *)
    number_on_disc : int;
    title : string;
    recording_title : string option;
    artists : Tag_artist.Collection.t;
    id : string }

let of_mb mb =
  let module T = Mb_track in
  let module R = Mb_recording in
  let id = mb.T.id
  and number = Option.value mb.T.position ~default:0 in
  let number_on_disc = number in

  let title, recording_title =
    match mb.T.title, mb.T.recording with
    | Some t, Some r ->
       begin match r.R.title with
       | Some rt ->
          if Ubase.from_utf8 t = Ubase.from_utf8 rt then
            (t, None)
          else
            (t, Some rt)
       | None -> (t, None)
       end
    | Some t, None -> (t, None)
    | None, Some r ->
       begin match r.R.title with
       | Some rt -> (rt, None)
       | None -> ("(untitled)", None)
       end
    | None, None -> ("(untitled)", None) in

  let artists =
    Tag_artist.of_credits mb.T.artist_credits in
  let artists =
    match mb.T.recording with
    | Some r -> Tag_artist.Collection.union (Tag_artist.of_credits r.R.artist_credits) artists
    | None -> artists in

  { id; number; number_on_disc; title; recording_title; artists }

let recording_title t =
  match t.recording_title with
  | Some title -> { t with title; recording_title = None }
  | None -> t


