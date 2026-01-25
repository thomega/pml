module Artist =
  struct

    module A = Musicbrainz.Artist

    type t =
      { name : string;
        artist_type : Artist_type.t;
        lifespan : Lifespan.t;
        id : string }

    let sort_name_of_name name =
      name

    let compare a1 a2 =
      let c = Artist_type.compare a1.artist_type a2.artist_type in
      if c <> 0 then
        c
      else
        let c = Lifespan.compare a1.lifespan a2.lifespan in
        if c <> 0 then
          c
        else
          let c = String.compare a1.name a2.name in
          if c <> 0 then
            c
          else
            String.compare a1.id a2.id

    let of_mb mb =
      let module A = Musicbrainz.Artist in
      let module AT = Artist_type in
      let id = mb.A.id
      and name =
        match mb.A.sort_name, mb.A.name with
        | Some sort_name, Some _name -> sort_name
        | Some sort_name, None -> sort_name
        | None, Some name -> sort_name_of_name name
        | None, None -> "(anonymous)"
      and artist_type =
        Option.value mb.A.artist_type ~default:(AT.Person AT.Roles.empty)
      and lifespan =
        Option.value mb.A.lifespan ~default:Lifespan.Limbo in
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

let artists_of_credits credits =
  List.filter_map
    (fun c -> Option.map Artist.of_mb c.Musicbrainz.Artist_Credit.artist)
    credits
  |> Artists.of_list

module Track =
  struct

    type t =
      { number : int;  (** Overall position of the track in the whole work, counting from 1. *)
        title : string;
        recording_title : string option;
        artists : Artists.t;
        id : string }

    let of_mb mb =
      let module T = Musicbrainz.Track in
      let module R = Musicbrainz.Recording in
      let id = mb.T.id
      and number = Option.value mb.T.position ~default:0 in

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

      let artists = artists_of_credits mb.T.artist_credits in
      let artists =
        match mb.T.recording with
        | Some r -> Artists.union (artists_of_credits r.R.artist_credits) artists
        | None -> artists in

      { id; number; title; recording_title; artists }

  end

module Medium =
  struct

    type t =
      { title : string option;
        tracks : Track.t list;
        id : string }

    let of_mb mb =
      let module MB = Musicbrainz.Medium in
      let id = mb.MB.id
      and title = mb.MB.title
      and tracks = List.map Track.of_mb mb.MB.tracks in
      { id; title; tracks }

  end

module Release =
  struct

    type t =
      { title : string option;
        artists : Artists.t;
        media : Medium.t list;
        id : string }

    let of_mb mb =
      let module MB = Musicbrainz.Release in
      let id = mb.MB.id
      and title = mb.MB.title
      and artists = artists_of_credits mb.MB.artist_credits
      and media = List.map Medium.of_mb mb.MB.media in
      { id; title; artists; media }

  end

module Disc =
  struct

    type title_options =
      { release : string option;
        medium : string option;
        tracks : string option }

    type t =
      { artist : Artist.t;
        titles : string list;
        title_options : title_options;
        performer : Artist.t option;
        tracks : Track.t list;
        tracks_orig : Track.t list option;
        total_tracks : int;
        discid : string }

    let prefix_track_titles tracks =
      List.map (fun t -> t.Track.title) tracks |> Edit.common_prefix

    let replace_track_titles strings tracks =
      List.map2 (fun s t -> { t with Track.title = s }) strings tracks

    let of_mb mb =
      let module MB = Musicbrainz.Taggable in
      let medium = Medium.of_mb mb.MB.medium
      and release = Release.of_mb mb.MB.release
      and discid = mb.MB.discid in
      let artist = Artists.min_elt release.Release.artists in
      let performer = Artists.min_elt_opt (Artists.remove artist release.Release.artists)
      and tracks = medium.Medium.tracks 
      and total_tracks = 100 in
      let tracks_title, tracks, tracks_orig =
        match prefix_track_titles tracks with
        | "" , _ -> (None, tracks, None)
        | pfx, tails -> (Some pfx, replace_track_titles tails tracks, Some tracks) in
      let title_options =
        { medium = medium.Medium.title;
          release = release.Release.title;
          tracks = tracks_title } in
      let titles =
        List.filter_map Fun.id [title_options.tracks; title_options.medium; title_options.release] in
      { artist; titles; title_options; performer; tracks; tracks_orig; total_tracks; discid }

    let print d =
      let open Printf in
      printf "Discid: %s\n" d.discid;
      begin match d.performer with
      | Some _ -> printf "Composer: %s\n" d.artist.Artist.name
      | None -> printf "Artist: %s\n" d.artist.Artist.name
      end;
      List.iter (fun t -> printf "Title: %s\n" t) d.titles;
      begin match d.title_options.tracks with
      | Some t -> printf "Tracks Prefix: %s\n" t
      | None -> ()
      end;
      begin match d.title_options.medium with
      | Some t -> printf "Medium: %s\n" t
      | None -> ()
      end;
      begin match d.title_options.release with
      | Some t -> printf "Release: %s\n" t
      | None -> ()
      end;
      begin match d.performer with
      | Some p -> printf "Performer: %s\n" p.Artist.name
      | None -> ()
      end;
      List.iter
        (fun t ->
          printf "  #%02d: '%s'\n" t.Track.number t.Track.title;
          begin match t.recording_title with
          | Some t -> printf "     = '%s'\n" t
          | None -> ()
          end;
          Artists.iter (fun a -> printf "       > %s\n" a.Artist.name) t.Track.artists)
        d.tracks

  end
