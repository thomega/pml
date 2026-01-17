module Artist =
  struct

    module MB = Musicbrainz.Artist

    type t =
      { name : string option;
        aliases : string list;
        artist_type : MB.artist_type;
        role : string option;
        id : string }

    let sort_name_of_name name =
      name

    let of_mb mb =
      let id = mb.MB.id in
      let name, aliases =
        match mb.MB.sort_name, mb.MB.name with
        | Some _ as sort_name, Some name -> (sort_name, [name])
        | Some _ as sort_name, None -> (sort_name, [])
        | None, Some name ->
           let sort_name = sort_name_of_name name in
           let aliases =
             if sort_name = name then
               []
             else
               [name] in
           (Some sort_name, aliases)
        | None, None -> (None, []) in
      let artist_type =
        Option.value mb.MB.artist_type ~default:MB.Person in
      let role =
        None in
      { id; name; aliases; artist_type; role }

  end

module Track =
  struct

    type t =
      { number : int;  (** Overall position of the track in the whole work, counting from 1. *)
        title : string;
        performers : Artist.t list;
        id : string }

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
