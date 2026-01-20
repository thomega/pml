module Artist =
  struct

    module MB = Musicbrainz.Artist
    module AT = Artist_type

    type t =
      { name : string;
        artist_type : AT.t;
        lifespan : Lifespan.t;
        id : string }

    let sort_name_of_name name =
      name

    let of_mb mb =
      let id = mb.MB.id
      and name =
        match mb.MB.sort_name, mb.MB.name with
        | Some sort_name, Some _name -> sort_name
        | Some sort_name, None -> sort_name
        | None, Some name -> sort_name_of_name name
        | None, None -> "(anonymous)"
      and artist_type =
        Option.value mb.MB.artist_type ~default:(AT.Person AT.Roles.empty)
      and lifespan =
        Option.value mb.MB.lifespan ~default:Lifespan.Limbo in
      { id; name; artist_type; lifespan }

  end

let disjoint_oldest_opt artists =
  let open Artist in
  match List.sort (fun a1 a2 -> Lifespan.compare a1.lifespan a2.lifespan) artists with
  | [] | [_] -> None
  | a1 :: (a2 :: _ as alist) ->
     begin match Lifespan.relation a1.lifespan a2.lifespan with
     | Before -> Some (a1, alist)
     | After | Overlap -> None 
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
