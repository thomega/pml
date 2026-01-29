type t =
  { id : string; (** While this is optional in the DTD, it should be there anyway. *)
    position : int option;
    title : string option;
    discs : Mb_disc.t list;
    tracks : Mb_track.t list }

let make id position title discs tracks =
  let discs = Option.value ~default:[] discs
  and tracks = Option.value ~default:[] tracks
  and title = Edit.blank_to_none title in
  { id; position; title; discs; tracks }

let jsont =
  Jsont.Object.map ~kind:"Medium" make
  |> Jsont.Object.mem "id" Jsont.string
  |> Jsont.Object.opt_mem "position" Jsont.int
  |> Jsont.Object.opt_mem "title" Jsont.string
  |> Jsont.Object.opt_mem "discs" Jsont.(list Mb_disc.jsont)
  |> Jsont.Object.opt_mem "tracks" Jsont.(list Mb_track.jsont)
  |> Jsont.Object.finish

let artist_ids m =
  List.map Mb_track.artist_ids m.tracks |> Sets.mbid_union

let update_artists map m =
  let open Result.Syntax in
  let* tracks = Result_list.map (Mb_track.update_artists map) m.tracks in
  Ok { m with tracks }

let print m =
  let open Printf in
  let n = Option.value m.position ~default:0 in
  printf "Disc %2d: %s\n"
    n
    (match m.title with
     | None | Some "" -> "[" ^ m.id ^ "]"
     | Some s -> s);
  List.iter (Mb_track.print n) m.tracks;
  ()
