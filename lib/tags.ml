module Artist =
  struct

    module MB = Musicbrainz.Artist
    module AT = Artist_type

    type t =
      { name : string;
        artist_type : AT.t;
        id : string }

    let sort_name_of_name name =
      name

    let of_mb mb =
      let id = mb.MB.id in
      let name =
        match mb.MB.sort_name, mb.MB.name with
        | Some sort_name, Some _name -> sort_name
        | Some sort_name, None -> sort_name
        | None, Some name -> sort_name_of_name name
        | None, None -> "(anonymous)" in
      let artist_type =
        Option.value mb.MB.artist_type ~default:(AT.Person AT.Roles.empty) in
      { id; name; artist_type }

  end

module All_tracks =
  struct
    type t =
      { title : string;
        composers : Artist.t list;
        performers : Artist.t list;
        tracks : int }
    let of_mb () =
      failwith "missing"
  end

module Track =
  struct

    type t =
      { number : int;  (** Overall position of the track in the whole work, counting from 1. *)
        title : string;
        performers : Artist.t list;
        inherited : All_tracks.t;
        id : string }

    let of_mb mb =
      let module MB = Musicbrainz.Track in
      let id = mb.MB.id in
      let number = 0 in
      let title = "" in
      let performers = [] in
      let inherited = All_tracks.of_mb () in
      { id; number; title; performers; inherited }

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
