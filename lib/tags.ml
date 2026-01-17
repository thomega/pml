module Artist =
  struct
    type t =
      { id : string;
        name : string;
        sort_name : string option;
        part : string option;
        role : string option }
    let of_mb mb =
      let module MB = Musicbrainz.Artist in
      let id = mb.MB.id in
      let name, sort_name =
        match mb.MB.name, mb.MB.sort_name with
        | Some name, sort_name -> (name, sort_name)
        | None, (Some name as sort_name) -> (name, sort_name)
        | None, None -> ("Anonymous", None) in
      let part = None in
      let role = None in
      { id; name; sort_name; part; role }
  end

module Track =
  struct
    type t =
      { id : string;
        number : int;  (** Overall position of the track in the whole work, counting from 1. *)
        title : string;
        performers : Artist.t list }
    let of_mb mb =
      let module MB = Musicbrainz.Track in
      let id = mb.MB.id in
      let number = 0 in
      let title = "" in
      let performers = [] in
      { id; number; title; performers }
  end

module Partial =
  struct
    type t =
      { release : string;
        disc : string;
        title : string;
        composers : Artist.t list;
        performers : Artist.t list;
        tracks : Track.t list;
        total_tracks : int }
  end
