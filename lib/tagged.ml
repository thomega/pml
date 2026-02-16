(* tagged.ml -- part of PML (Physical Media Library)

   Copyright (C) 2026 by Thorsten Ohl <ohl@physik.uni-wuerzburg.de>

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>. *)

type title =
  | User of string
  | Tracks of string
  | Medium of string
  | Release of string

let title_kind_to_string = function
  | User _ -> "user choice"
  | Tracks _ -> "track prefix"
  | Medium _ -> "medium"
  | Release _ -> "release"

let title_to_string = function
  | User s | Tracks s | Medium s | Release s -> s

module Artists = Artist.Collection

type multi =
  { tracks' : Track.t list;
    tracks_mb : Track.t list option }

type tracks =
  | Single of Track.t
  | Multi of multi

type t =
  { composer : Artist.t option;
    titles : title list;
    performer : Artist.t option;
    artists : Artists.t;
    tracks : tracks;
    track_width : int;
    discid : string;
    medium_title : string option;
    medium_id : string;
    release_title : string option;
    release_id : string }

(** TODO: sanitize titles for filenames, rotate articles, ... *)

(** Heuristics for selecting composer and top billed performer.
    This relies on the ordering in [Artist_types]. *)
let make_artists artists =
  match Artists.min_elt_opt artists with
  | None -> (None, None)
  | Some composer ->
     let performer_opt = Artists.min_elt_opt (Artists.remove composer artists) in
     (Some composer, performer_opt)

let replace_track_titles strings tracks =
  List.map2 (fun s t -> { t with Track.title = s }) strings tracks

let white = " \t\n\r"
let punctuation = ":;.-_/"
let white_or_punctuation = white ^ punctuation

let re_trailing_punctuation =
  Re.(seq [rep1 (set white_or_punctuation); stop] |> compile)

let re_leading_punctuation =
  Re.(seq [start; rep1 (set white_or_punctuation)] |> compile)

let strip_trailing_punctuation s =
  Re.replace_string re_trailing_punctuation ~by:"" s

let strip_leading_punctuation s =
  Re.replace_string re_leading_punctuation ~by:"" s

(** TODO: sanitize titles for filenames, rotate articles, ... *)

let%test_module _ =
  (module struct

     let%test _ = strip_trailing_punctuation "abc: " = "abc"
     let%test _ = strip_trailing_punctuation "abc - /" = "abc"
     let%test _ = strip_trailing_punctuation "abc: def: " = "abc: def"

   end)

(** Heuristics for selecting the title. *)
let make_titles ~release_title ~medium_title = function
  | Multi multi ->
     let prefix, tracks', tracks_mb =
       match List.map (fun t -> t.Track.title) multi.tracks' |> Edit.common_prefix with
       | "" , _ ->
          (None, multi.tracks', None)
       | pfx, tails ->
          let title = strip_trailing_punctuation pfx
          and tails = List.map strip_leading_punctuation tails in
          let stripped_tracks = replace_track_titles tails multi.tracks' in
          (Some title, stripped_tracks, Some multi.tracks') in
     let titles =
       List.filter_map Fun.id
         [ Option.map (fun t -> Tracks t) prefix;
           Option.map (fun t -> Medium t) medium_title;
           Option.map (fun t -> Release t) release_title ] in
     (titles, Multi { tracks'; tracks_mb } )
  | Single track ->
     let titles =
       Tracks track.Track.title ::
         (List.filter_map Fun.id
            [ Option.map (fun t -> Medium t) medium_title;
              Option.map (fun t -> Release t) release_title ]) in
     (titles, Single track)

let refresh_titles tracks d =
  let release_title = d.release_title
  and medium_title = d.medium_title in
  make_titles ~release_title ~medium_title tracks

let add_tracks_artists artists tracks =
  List.fold_left
    (fun acc track -> Artists.union acc track.Track.artists)
    artists tracks

let common_tracks_artists = function
  | [] -> Artists.empty
  | track :: tracks ->
     List.fold_left
       (fun acc track -> Artists.inter acc track.Track.artists)
       track.Track.artists tracks

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
  let tracks = Multi { tracks' = medium.Medium.tracks; tracks_mb = None } in
  let titles, tracks =
    make_titles ~release_title ~medium_title tracks in
  { composer; titles; performer; artists;
    tracks; track_width;
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

type trackset =
  { offset : int;
    first : int;
    last : int option;
    width : int;
    single : bool }

let default_trackset =
  { offset = 0; first = 1; last = None; width = 2; single = false }

let select_tracklist subset tracks =
  sublist subset.first subset.last tracks
  |> List.map (fun t -> Track.{ t with number = t.number + subset.offset - subset.first + 1 })

let tracks_mb = function
  | Multi multi ->
     begin match multi.tracks_mb with
     | Some tracks -> tracks
     | None -> multi.tracks'
     end
  | Single track -> [track]

let edited = function
  | Multi multi -> multi.tracks'
  | Single track -> [track]

let tracks' d =
  edited d.tracks

let multi tracks' =
  Multi { tracks'; tracks_mb = None }

let select_tracks subset d =
  let open Result.Syntax in
  let track_width = subset.width in
  let tracks' = tracks_mb d.tracks |> select_tracklist subset in
  let* tracks =
    if subset.single then
      match tracks' with
      | [t] -> Ok (Single t)
      | _ -> Error (Printf.sprintf "--single requires a single track, not %d" (List.length tracks'))
    else
      Ok (multi tracks') in
  let titles, tracks = refresh_titles tracks d in
  let composer, performer = common_tracks_artists tracks' |> make_artists in
  Ok { d with titles; composer; performer; tracks; track_width }

let map_tracks f = function
  | Multi multi ->
     let tracks' = List.map f multi.tracks' in
     Multi { multi with tracks' }
  | Single single ->
     Single (f single)

let map_tracks_result f tracks =
  let open Result.Syntax in
  match tracks with
  | Multi multi ->
     let* tracks' = Result_list.map f multi.tracks' in
     Ok (Multi { multi with tracks' })
  | Single track ->
     let* track = f track in
     Ok (Single track)

let filter_artists range predicate t =
  let artists = Artist.Collection.filter predicate t.artists
  and tracks =
    map_tracks
      (fun track ->
        if Edit.in_range track.Track.number range then
          Track.filter_artists predicate track
        else
          track)
      t.tracks in
  let artists = add_tracks_artists artists (edited tracks) in
  let composer, performer = make_artists artists in
  { t with composer; performer; artists; tracks  }

let edit_artists (range, perl_s) t =
  let open Result.Syntax in
  let* tracks =
    map_tracks_result
      (fun track ->
        if Edit.in_range track.Track.number range then
          Track.map_artists
            (fun a ->
              let* name = Perl.S.exec perl_s a.Artist.name in
              Ok { a with name } )
            track
        else
          Ok track)
      t.tracks in
  let artists = add_tracks_artists t.artists (edited tracks) in
  let composer, performer = make_artists artists in
  Ok { t with composer; performer; artists; tracks  }

let add_artist (range, name) t =
  let open Result.Syntax in
  let* tracks =
    map_tracks_result
      (fun track ->
        if Edit.in_range track.Track.number range then
          let artists =
            Artist.Collection.add (Artist.of_name name) track.Track.artists in
          Ok { track with artists }
        else
          Ok track)
      t.tracks in
  let artists = add_tracks_artists t.artists (edited tracks) in
  let composer, performer = make_artists artists in
  Ok { t with composer; performer; artists; tracks  }

let edit_track_titles (range, perl_s) d =
  let open Result.Syntax in
  let* tracks =
    map_tracks_result
      (fun track ->
        if Edit.in_range track.Track.number range then
          let* title = Perl.S.exec perl_s track.Track.title in
          Ok { track with title }
        else
          Ok track)
      d.tracks in
  let titles, tracks = refresh_titles tracks d in
  Ok { d with titles; tracks }

let recording_titles d =
  let tracks' = tracks_mb d.tracks |> List.map Track.recording_title in
  let tracks = multi tracks' in
  let titles, tracks = refresh_titles tracks d in
  Ok { d with titles; tracks }

let force_user_title title d =
  begin match d.tracks with
  | Single _ -> assert false
  | Multi _ -> ()
  end;
  let titles = User title :: d.titles
  and tracks' = tracks_mb d.tracks
  and tracks_mb = None in
  let tracks = Multi { tracks'; tracks_mb } in
  { d with titles; tracks }

let chop_prefix n s =
  String.sub s n (String.length s - n) |> strip_leading_punctuation

let chop_prefixes n tracks =
  List.map (fun t -> { t with Track.title = chop_prefix n t.Track.title }) tracks

let user_title title d =
  let title = strip_trailing_punctuation title in
  match d.titles with
  | Tracks longest_prefix :: _ when String.starts_with ~prefix:title longest_prefix ->
     let titles = User title :: d.titles in
     let n = String.length title in
     begin match d.tracks with
     | Single _ -> 
        Ok { d with titles }
     | Multi multi ->
        let tracks', tracks_mb =
          begin match multi.tracks_mb with
          | None -> (chop_prefixes n multi.tracks', Some multi.tracks')
          | Some tracks_mb -> (chop_prefixes n tracks_mb, Some tracks_mb)
          end in
        let tracks = Multi { tracks'; tracks_mb } in
        Ok { d with titles; tracks }
     end
  | _ -> Ok (force_user_title title d)

let edit_prefix sub d =
  match d.titles with
  | Tracks longest_prefix :: _ ->
     let open Result.Syntax in
     let* title =
       try
         Perl.S.exec sub longest_prefix
       with
       | e -> Error (Printexc.to_string e) in
     if String.starts_with ~prefix:title longest_prefix then
       let titles = User title :: d.titles in
       let n = String.length title in
       begin match d.tracks with
       | Single _ -> 
          Ok { d with titles }
       | Multi multi ->
          let tracks', tracks_mb =
            begin match multi.tracks_mb with
            | None -> (chop_prefixes n multi.tracks', Some multi.tracks')
            | Some tracks_mb -> (chop_prefixes n tracks_mb, Some tracks_mb)
            end in
          let tracks = Multi { tracks'; tracks_mb } in
          Ok { d with titles; tracks }
       end
     else
       Ok d
  | _ -> Ok d

let edit_title sub d =
  match d.titles with
  | User title :: _ | Tracks title :: _ | Medium title :: _ | Release title :: _ ->
     let open Result.Syntax in
     let* title =
       try
         Perl.S.exec sub title
       with
       | e -> Error (Printexc.to_string e) in
     let titles = User title :: d.titles in
     Ok { d with titles }
  | _ -> Ok d

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

let delete_artists (range, perl_m) d =
  Ok (filter_artists range (fun a -> Perl.M.exec perl_m a.Artist.name |> not) d)

let delete_artists_sort (range, perl_m) d =
  Ok (filter_artists range (fun a -> Perl.M.exec perl_m a.Artist.sort_name |> not) d)

let user_composer name d =
  Ok { d with composer = Some (Artist.of_sort_name name) }

let user_performer name d =
  Ok { d with performer = Some (Artist.of_sort_name name) }

(** NB: we can't use [Set().find_first_opt] as a shortcut, because
    it requires a monotonically increasing function as predicate. *)
let match_artist rex d =
  let matches = Artists.filter (fun a -> Perl.M.exec rex a.Artist.name) d.artists in
  match Artists.min_elt_opt matches with
  | Some a -> Ok a.Artist.sort_name
  | None -> Error (Printf.sprintf "pattern '%s' matches no artist" (Perl.M.to_string rex))

let composer_pattern rex d =
  let open Result.Syntax in
  let* name = match_artist rex d in
  user_composer name d

let performer_pattern rex d =
  let open Result.Syntax in
  let* name = match_artist rex d in
  user_performer name d

module Edits =
  struct

    type all =
      { trackset : trackset option;
        recording_titles : bool;
        edit_track_titles : Perl.S.ranged list;
        release_title : bool;
        medium_title : bool;
        title : string option;
        edit_prefix : Perl.S.t list;
        edit_title : Perl.S.t list;
        delete_artists : Perl.M.ranged list;
        delete_artists_sort : Perl.M.ranged list;
        edit_artists : Perl.S.ranged list;
        add_artist : string Edit.ranged list;
        composer_pattern : Perl.M.t option;
        performer_pattern : Perl.M.t option;
        composer : string option;
        performer : string option }

    let apply f opt tagged =
      let open Result.Syntax in
      let* tagged in
      match opt with
      | None -> Ok tagged
      | Some s -> f s tagged

    let apply_if flag f tagged =
      let open Result.Syntax in
      let* tagged in
      if flag then
        f tagged
      else
        Ok tagged

    let apply_pcre_s f pcre_list tagged =
      let open Result.Syntax in
      let* tagged in
      Result_list.fold_left (fun acc sub -> f sub acc) tagged pcre_list

    let apply_pcre_m f pcre_list tagged =
      let open Result.Syntax in
      let* tagged in
      Result_list.fold_left (fun acc rex -> f rex acc) tagged pcre_list

    let apply_names f ranged_names tagged =
      let open Result.Syntax in
      let* tagged in
      Result_list.fold_left (fun acc sub -> f sub acc) tagged ranged_names

    (** The order is very significant! *)
    let apply_all e tagged =
      Ok tagged
      |> apply select_tracks e.trackset
      |> apply_if e.recording_titles recording_titles
      |> apply_pcre_s edit_track_titles e.edit_track_titles
      |> apply_if e.release_title release_title
      |> apply_if e.medium_title medium_title
      |> apply user_title e.title
      |> apply_pcre_s edit_prefix e.edit_prefix
      |> apply_pcre_s edit_title e.edit_title
      |> apply_pcre_m delete_artists e.delete_artists
      |> apply_pcre_m delete_artists_sort e.delete_artists_sort
      |> apply_pcre_s edit_artists e.edit_artists
      |> apply_names add_artist e.add_artist
      |> apply composer_pattern e.composer_pattern
      |> apply performer_pattern e.performer_pattern
      |> apply user_composer e.composer
      |> apply user_performer e.performer

  end

let list_artists lcw artists =
  match Artists.min_elt_opt artists with
  | None -> ()
  | Some artist ->
     Printf.printf "%*s '%s'\n" lcw "Artists:" (Artist.to_string artist);
     Artists.iter
       (fun a -> Printf.printf "%*s '%s'\n" lcw "" (Artist.to_string a))
       (Artists.remove artist artists)

let artist_intersection = function
  | [] -> Artists.empty
  | [t] -> t.Track.artists
  | t :: tlist ->
     List.fold_left (fun acc t -> Artists.inter acc t.Track.artists) t.Track.artists tlist

let factor_track_artists d =
  let common = artist_intersection (tracks' d) in
  let tracks =
    map_tracks (fun t -> Track.{ t with artists = Artists.diff t.artists common }) d.tracks in
  let artists = Artists.union d.artists common in
  { d with artists; tracks }

let target_dir d =
  let root =
    match d.composer with
    | Some c -> c.Artist.sort_name
    | None -> "Anonymous"
  and subdir =
    match d.titles, d.performer with
    | [], None -> "Unnamed"
    | t :: _, None -> title_to_string t
    | [], Some p -> p.Artist.sort_name
    | t :: _, Some p -> title_to_string t ^ " - " ^ p.Artist.sort_name in
  let root = Edit.filename_safe root
  and subdir = Edit.filename_safe subdir in
  (root, Filename.concat root subdir)

let print ?(no_artists=false) ?(factor_artists=false) ?(no_originals=false) ?(no_recordings=false) d =
  let open Printf in
  let lcw = 10 in
  printf "Discid:  %s\n" d.discid;
  printf "Medium:  %s\n" d.medium_id;
  printf "Release: %s\n\n" d.release_id;
  let d =
    if factor_artists then
      factor_track_artists d
    else
      d in
  begin match d.composer, d.performer with
  | Some composer, Some performer ->
     printf "%-*s '%s'\n" lcw "Composer:" (Artist.to_string composer);
     printf "%-*s '%s'\n" lcw "Performer:" (Artist.to_string performer)
  | None, Some performer ->
     printf "%-*s '%s'\n" lcw "Performer:" (Artist.to_string performer)
  | Some composer, None ->
     printf "%-*s '%s'\n" lcw "Artist:" (Artist.to_string composer)
  | None, None -> ()
  end;
  printf "\nTitle:\n\n";
  let kind_width =
    List.fold_left (fun acc t -> max acc (String.length (title_kind_to_string t))) 0 d.titles in
  List.iter
    (fun t ->
      printf "%*s '%s'\n" (kind_width + 3) (title_kind_to_string t ^ ":") (title_to_string t))
    d.titles;
  if not no_artists then (printf "\n"; list_artists lcw d.artists);
  let _, dir = target_dir d in
  printf "\n%-*s '%s'\n\n" lcw "Directory:" dir;
  let title =
    match d.titles with
    | [] -> "Unnamed"
    | t :: _ -> title_to_string t in
  begin match d.tracks with
  | Single t ->
     printf "%*s '%s'\n" lcw "Track:" title;
     printf "%*s '%s'\n" lcw "*Track:" t.Track.title;
     if not no_recordings then Option.iter (printf "%*s '%s'\n" lcw "rec.:") t.recording_title;
     if not no_artists then list_artists lcw t.Track.artists
  | Multi multi ->
     begin match multi.tracks_mb with
     | None ->
        List.iter
          (fun t ->
            printf "%-*s %0*d: '%s'\n" (lcw - d.track_width - 2) "Track"
              d.track_width t.Track.number t.Track.title;
            if not no_recordings then Option.iter (printf "%*s '%s'\n" lcw "rec.:") t.recording_title;
            if not no_artists then list_artists lcw t.Track.artists)
          multi.tracks'
     | Some tracks_mb ->
        List.iter2
          (fun t ft ->
            printf "%-*s %0*d: '%s'\n" (lcw - d.track_width - 2) "Track"
              d.track_width t.Track.number t.Track.title;
            if not no_originals then printf "%*s '%s'\n" lcw "MB:" ft.Track.title;
            if not no_recordings then Option.iter (printf "%*s '%s'\n" lcw "rec.:") t.recording_title;
            if not no_artists then list_artists lcw t.Track.artists)
          multi.tracks' tracks_mb
     end
  end

