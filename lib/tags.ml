module Medium =
  struct

    type t =
      { title : string option;
        tracks : Tag_track.t list;
        id : string }

    let of_mb mb =
      let module M = Mb_medium in
      let id = mb.M.id
      and title = mb.M.title
      and tracks = List.map Tag_track.of_mb mb.M.tracks in
      { id; title; tracks }

  end

module Release =
  struct

    type t =
      { title : string option;
        artists : Tag_artist.Collection.t;
        media : Medium.t list;
        id : string }

    let of_mb mb =
      let module R = Mb_release in
      let id = mb.R.id
      and title = mb.R.title
      and artists = Tag_artist.of_credits mb.R.artist_credits
      and media = List.map Medium.of_mb mb.R.media in
      { id; title; artists; media }

  end

module Disc =
  struct

    type title =
      | User of string
      | Tracks of string
      | Medium of string
      | Release of string

    type trackset =
      { offset : int;
        first : int;
        last : int option;
        width : int }

    let default_trackset =
      { offset = 0; first = 1; last = None; width = 2 }

    let title_kind_to_string = function
      | User _ -> "user"
      | Tracks _ -> "tracks"
      | Medium _ -> "medium"
      | Release _ -> "release"

    let title_to_string = function
      | User s | Tracks s | Medium s | Release s -> s

    type t =
      { composer : Tag_artist.t option;
        titles : title list;
        performer : Tag_artist.t option;
        artists : Tag_artist.Collection.t;
        tracks : Tag_track.t list;
        tracks_orig : Tag_track.t list option;
        track_width : int;
        discid : string;
        medium_title : string option;
        medium_id : string;
        release_title : string option;
        release_id : string }

    (** TODO: sanitize titles for filenames, rotate articles, ... *)

    (** Heuristics for selecting composer and top billed performer.
        This relies on the ordering in [Tag_artist_types]. *)
    let make_artists artists =
      match Tag_artist.Collection.min_elt_opt artists with
      | None -> (None, None)
      | Some composer ->
         let performer_opt = Tag_artist.Collection.min_elt_opt (Tag_artist.Collection.remove composer artists) in
         (Some composer, performer_opt)

    let replace_track_titles strings tracks =
      List.map2 (fun s t -> { t with Tag_track.title = s }) strings tracks

    let punctuation = ":;.-_/"

    let re_trailing_punctuation =
      Re.(seq [rep1 (alt [blank; set punctuation]); stop] |> compile)

    let re_leading_punctuation =
      Re.(seq [start; rep1 (alt [blank; set punctuation])] |> compile)

    let strip_trailing_punctuation s =
      Re.replace_string re_trailing_punctuation ~by:"" s

    let strip_leading_punctuation s =
      Re.replace_string re_leading_punctuation ~by:"" s

    (** TODO: sanitize titles for filenames, rotate articles, ... *)

    let%test_module _ =
      (module struct

         let%test _ =
           strip_trailing_punctuation "abc: " = "abc"

         let%test _ =
           strip_trailing_punctuation "abc - /" = "abc"

         let%test _ =
           strip_trailing_punctuation "abc: def: " = "abc: def"

       end)

    (** Heuristics for selecting the title. *)
    let make_titles ~release_title ~medium_title tracks =
      let prefix, tracks, tracks_orig =
        match List.map (fun t -> t.Tag_track.title) tracks |> Edit.common_prefix with
        | "" , _ ->
           (None, tracks, None)
        | pfx, tails ->
           let title = strip_trailing_punctuation pfx
           and tails = List.map strip_leading_punctuation tails in
           let stripped_tracks = replace_track_titles tails tracks in
           (Some title, stripped_tracks, Some tracks) in
      let titles =
        List.filter_map Fun.id
          [ Option.map (fun t -> Tracks t) prefix;
            Option.map (fun t -> Medium t) medium_title;
            Option.map (fun t -> Release t) release_title ] in
      (titles, tracks, tracks_orig)

    let add_tracks_artists artists tracks =
      List.fold_left
        (fun acc track -> Tag_artist.Collection.union acc track.Tag_track.artists)
        artists tracks

    let of_mb mb =
      let medium = Medium.of_mb mb.Taggable.medium
      and release = Release.of_mb mb.Taggable.release
      and discid = mb.Taggable.discid
      and track_width = 2 in
      let medium_id = medium.Medium.id
      and medium_title = medium.Medium.title
      and release_id = release.Release.id
      and release_title = release.Release.title in
      let artists = add_tracks_artists release.Release.artists medium.Medium.tracks in
      let composer, performer = make_artists artists in
      let titles, tracks, tracks_orig =
        make_titles ~release_title ~medium_title medium.Medium.tracks in
      { composer; titles; performer; artists;
        tracks; tracks_orig; track_width;
        discid; medium_id; medium_title; release_id; release_title }

    let sublist first last l =
      let l = List.drop (pred first) l in
      match last with
      | Some last -> List.take (last - first + 1) l
      | None -> l

    (** Test for all possible off-by-one errors. *)

    let%test_module _ =
      (module struct

         let%test _ = sublist 1 None [] = []
         let%test _ = sublist 0 None [] = []
         let%test _ = sublist (-1) None [] = []
         let%test _ = sublist 1 None [1] = [1]
         let%test _ = sublist 0 None [1] = [1]
         let%test _ = sublist 2 None [1] = []
         let%test _ = sublist 9 None [1] = []
         let%test _ = sublist 1 None [1;2] = [1;2]
         let%test _ = sublist 0 None [1;2] = [1;2]
         let%test _ = sublist 2 None [1;2] = [2]
         let%test _ = sublist 3 None [1;2] = []

         let%test _ = sublist 1 (Some 3) [] = []
         let%test _ = sublist 1 (Some 1) [1] = [1]
         let%test _ = sublist 1 (Some 0) [1] = []
         let%test _ = sublist 1 (Some 2) [1] = [1]
         let%test _ = sublist 1 (Some 2) [1;2;3] = [1;2]
         let%test _ = sublist 2 (Some 2) [1;2;3] = [2]
         let%test _ = sublist 2 (Some 3) [1;2;3] = [2;3]

       end)

    let select_tracklist subset tracks =
      sublist subset.first subset.last tracks
      |> List.map (fun t -> Tag_track.{ t with number = t.number + subset.offset - subset.first + 1 })

    let orig_tracks d =
      match d.tracks_orig with
      | Some tracks -> tracks
      | None -> d.tracks

    let select_tracks subset d =
      let release_title = d.release_title
      and medium_title = d.medium_title
      and track_width = subset.width in
      let tracks = orig_tracks d |> select_tracklist subset in
      let titles, tracks, tracks_orig =
        make_titles ~release_title ~medium_title tracks in
      Ok { d with titles; tracks; tracks_orig; track_width }

    let recording_titles d =
      let release_title = d.release_title
      and medium_title = d.medium_title in
      let tracks = orig_tracks d |> List.map Tag_track.recording_title in
      let titles, tracks, tracks_orig =
        make_titles ~release_title ~medium_title tracks in
      Ok { d with titles; tracks; tracks_orig }

    let force_user_title title d =
      let titles = [User title]
      and tracks = orig_tracks d
      and tracks_orig = None in
      { d with titles; tracks; tracks_orig }

    let chop_prefix n s =
      String.sub s n (String.length s - n) |> strip_leading_punctuation

    let chop_prefixes n tracks =
      List.map (fun t -> { t with Tag_track.title = chop_prefix n t.Tag_track.title }) tracks

    let user_title title d =
      let title = strip_trailing_punctuation title in
      match d.titles with
      | Tracks longest_prefix :: _ when String.starts_with ~prefix:title longest_prefix ->
         let titles = [User title] in
         let tracks, tracks_orig =
           let n = String.length title in
           match d.tracks_orig with
           | None -> (chop_prefixes n d.tracks, Some d.tracks)
           | Some tracks_orig -> (chop_prefixes n tracks_orig, d.tracks_orig) in
         Ok { d with titles; tracks; tracks_orig }
      | _ -> Ok (force_user_title title d)

    let get_medium_title d =
      match List.find_opt (function Medium _ -> true | _ -> false) d.titles with
      | Some t -> Ok (title_to_string t)
      | None -> Error "no medium title"
        
    let get_release_title d =
      match List.find_opt (function Release _ -> true | _ -> false) d.titles with
      | Some t -> Ok (title_to_string t)
      | None -> Error "no release title"
        
    let medium_title d =
      let open Result.Syntax in
      let* title = get_medium_title d in
      user_title title d

    let release_title d =
      let open Result.Syntax in
      let* title = get_release_title d in
      user_title title d

    let user_composer name d =
      Ok { d with composer = Some (Tag_artist.of_name name) }

    let user_performer name d =
      Ok { d with performer = Some (Tag_artist.of_name name) }

    let match_performer pfx d =
      let prefix = String.lowercase_ascii (Ubase.from_utf8 pfx) in
      let artist =
        Tag_artist.Collection.find_first_opt
          (fun a ->
            String.starts_with ~prefix (String.lowercase_ascii (Ubase.from_utf8 a.name)))
          d.artists in
      match artist with
      | Some a -> Ok a.name
      | None -> Error (Printf.sprintf "pattern '%s' matches no artist" pfx)

    let composer_prefix pfx d =
      let open Result.Syntax in
      let* name = match_performer pfx d in
      user_composer name d

    let performer_prefix pfx d =
      let open Result.Syntax in
      let* name = match_performer pfx d in
      user_performer name d

    (** We use a prefix for the WAV file, because discids can start with
        a period. *)
    let wav_prefix = "cd-"

    let script d =
      let open Printf in
      let wav_name i = sprintf "%s%s%02d.wav" wav_prefix d.discid i in
      let separator () =
        printf "########################################################################\n" in
      printf "#! /bin/sh\n";
      separator ();
      printf "DISCID='%s'\n" d.discid;
      printf "MEDIUM='%s'\n" d.medium_id;
      printf "RELEASE='%s'\n" d.release_id;
      separator ();
      printf "\n";
      separator ();
      printf "# Rip CD track unless the output file exists\n";
      separator ();
      printf "\n";
      printf "rip_track () {\n";
      printf "  if [ ! -r $2 ]; then\n";
      printf "    cdparanoia -w $1 $2\n";
      printf "  fi\n";
      printf "}\n";
      printf "\n";
      List.iter
        (fun t ->
          let n = t.Tag_track.number_on_disc in
          printf "rip_track %2d %s\n" n (wav_name n))
        d.tracks;
      printf "\n";
      separator ();
      printf "# Set up target directory\n";
      separator ();
      printf "\n";
      let root =
        match d.composer with
        | Some c -> c.Tag_artist.name
        | None -> "Anonymous" in
      printf "ROOT=\"%s\"\n" root;
      let subdir =
        match d.titles, d.performer with
        | [], None -> "Unnamed"
        | t :: _, None -> title_to_string t
        | [], Some p -> p.Tag_artist.name
        | t :: _, Some p -> sprintf "%s - %s" (title_to_string t) p.Tag_artist.name in
      printf "SUBDIR=\"%s\"\n" subdir;
      printf "DIR=\"$ROOT/$SUBDIR\"\n";
      printf "mkdir -p \"$DIR\"\n";
      printf "\n";
      separator ();
      printf "# Encode and tag\n";
      separator ();
      printf "\n";
      List.iter
        (fun t ->
          printf "WAV=%s\n" (wav_name t.Tag_track.number_on_disc);
          printf "TITLE=\"%0*d %s\"\n" d.track_width t.Tag_track.number t.Tag_track.title;
          printf "\n")
        d.tracks;
      Ok ()

    let print d =
      let open Printf in
      printf "Discid: '%s'\n" d.discid;
      printf "Medium: '%s'\n" d.medium_id;
      printf "Release: '%s'\n" d.release_id;
      begin match d.composer with
      | Some composer ->
         begin match d.performer with
         | Some _ -> printf "Composer: '%s'\n" composer.Tag_artist.name
         | None -> printf "Artist: '%s'\n" composer.Tag_artist.name
         end
      | None -> ()
      end;
      List.iter
        (fun t ->
          printf "Title(%s): '%s'\n" (title_kind_to_string t) (title_to_string t))
        d.titles;
      begin match d.performer with
      | Some p -> printf "Performer: '%s'\n" p.Tag_artist.name
      | None -> ()
      end;
      Tag_artist.Collection.iter (fun a -> printf "            > '%s'\n" a.Tag_artist.name) d.artists;
      begin match d.tracks_orig with
      | Some tracks_orig ->
         List.iter2
           (fun t ot ->
             printf "  #%0*d: '%s'\n" d.track_width t.Tag_track.number t.Tag_track.title;
             printf "       original:  '%s'\n" ot.Tag_track.title;
             begin match t.recording_title with
             | Some t -> printf "       recording: '%s'\n" t
             | None -> ()
             end;
             Tag_artist.Collection.iter (fun a -> printf "       > '%s'\n" a.Tag_artist.name) t.Tag_track.artists)
           d.tracks tracks_orig
      | None ->
         List.iter
           (fun t ->
             printf "  #%0*d: '%s'\n" d.track_width t.Tag_track.number t.Tag_track.title;
             begin match t.recording_title with
             | Some t -> printf "     = '%s'\n" t
             | None -> ()
             end;
             Tag_artist.Collection.iter (fun a -> printf "       > '%s'\n" a.Tag_artist.name) t.Tag_track.artists)
           d.tracks
      end

  end
