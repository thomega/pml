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

    let compare a1 a2 =
      let c = AT.compare a1.artist_type a2.artist_type in
      if c <> 0 then
        c
      else
        let c = Lifespan.compare a1.lifespan a2.lifespan in
        if c <> 0 then
          c
        else
          String.compare a1.name a2.name

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

module Artists = Set.Make (struct type t = Artist.t let compare = Artist.compare end)

let find_gaps is_gap sorted_list =
  let open List in
  let rec find_gaps' groups_rev group_rev = function
    | [] -> rev_append groups_rev [rev group_rev]
    | [_] as a -> rev_append groups_rev [rev_append group_rev a]
    | a1 :: (a2 :: _ as a2_etc) ->
       if is_gap a1 a2 then
         find_gaps' (rev_append group_rev [a1] :: groups_rev) [] a2_etc
       else
         find_gaps' groups_rev (a1 :: group_rev) a2_etc in
  find_gaps' [] [] sorted_list

let%test_module _ =
  (module struct

     let is_gap x y = y > x + 1

     let test_gaps alist groups =
       find_gaps is_gap alist = groups

     let%test _ = test_gaps [] [[]]
     let%test _ = test_gaps [1] [[1]]
     let%test _ = test_gaps [1;2] [[1;2]]
     let%test _ = test_gaps [1;3] [[1];[3]]
     let%test _ = test_gaps [1;3;5] [[1];[3];[5]]
     let%test _ = test_gaps [1;2;4;5;6;8;9] [[1;2];[4;5;6];[8;9]]
     let%test _ = test_gaps [1;3;2] [[1];[3;2]]

   end)

let lifespan_gaps artists =
  let open Artist in
  Artists.elements artists
  |> List.sort (fun a1 a2 -> Lifespan.compare a1.lifespan a2.lifespan)
  |> find_gaps
       (fun a1 a2 ->
         match Lifespan.relation a1.lifespan a2.lifespan with
         | Before -> true
         | After | Overlap -> false)
  |> List.map Artists.of_list

module All_tracks =
  struct
    type t =
      { title : string;
        composers : Artists.t;
        performers : Artists.t;
        tracks : int }
    let of_mb () =
      failwith "missing"
  end

module Track =
  struct

    type t =
      { number : int;  (** Overall position of the track in the whole work, counting from 1. *)
        title : string;
        composers : Artists.t;
        performers : Artists.t;
        all_tracks : All_tracks.t;
        id : string }

    let artists_of_credits credits =
      List.filter_map
        (fun c -> Option.map Artist.of_mb c.Musicbrainz.Artist_Credit.artist)
        credits

    let classify_artists credits =
      let artists = artists_of_credits credits in
      (Artists.of_list artists, Artists.of_list artists)

    let of_mb mb =
      let module MB = Musicbrainz.Track in
      let id = mb.MB.id
      and number = Option.value mb.MB.position ~default:0
      and title = Option.value mb.MB.title ~default:"(untitled)"
      and composers, performers = classify_artists mb.MB.artist_credits in
      let all_tracks = All_tracks.of_mb () in
      { id; number; title; composers; performers; all_tracks }

  end

module Partial =
  struct
    type t =
      { release : string;
        disc : string;
        title : string;
        composers : Artists.t;
        performers : Artists.t;
        tracks : Track.t list;
        total_tracks : int }
  end
