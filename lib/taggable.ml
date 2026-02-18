(* taggable.ml -- part of PML (Physical Media Library)

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

open Result.Syntax

type t =
  { medium : Mb_medium.t;
    release : Mb_release.t;
    discid : string }

let release_of_mbid ~root mbid =
  let* text = Cached.Release.get ~root mbid in
  Jsont_bytesrw.decode_string Mb_release.jsont text

let contains_discid discid medium =
  List.exists (fun disc -> discid = disc.Mb_disc.id) medium.Mb_medium.discs

let discs_of_discid ~root discid =
  let* releases = Cached.releases_of_discid ~root discid in
  let* discs =
    Result_list.map
      (fun mbid ->
        let* release = release_of_mbid ~root mbid in
        let media = List.filter (contains_discid discid) release.Mb_release.media in
        Ok (List.map (fun medium -> { medium; release; discid }) media))
      releases in
  match List.concat discs with
  | [] -> Error (Printf.sprintf
                   "no disc for discid '%s' in releases %s"
                   discid (String.concat ", " releases))
  | disc :: other_discs -> Ok (disc, other_discs)

let artist_ids d =
  Sets.MBID.union
    (Mb_medium.artist_ids d.medium)
    (Mb_release.artist_ids d.release)

let update_artists map d =
  let* medium = Mb_medium.update_artists map d.medium
  and* release = Mb_release.update_artists map d.release in
  Ok { d with medium; release }

let add_lifespans ~root disc =
  let ids = artist_ids disc |> Sets.MBID.elements in
  let* artist_map =
    Cached.Artist.map_of_ids ~root (Jsont_bytesrw.decode_string Mb_artist.jsont) ids in
  update_artists artist_map disc

let add_lifespans_local ~root disc =
  let ids = artist_ids disc |> Sets.MBID.elements in
  let* artist_map =
    Cached.Artist.map_of_ids_local ~root (Jsont_bytesrw.decode_string Mb_artist.jsont) ids in
  update_artists artist_map disc

let truncate n s =
  let l = String.length s in
  if l <= n then
    s
  else if n >= 3 then
    String.sub s 0 (n - 3) ^ "..."
  else
    invalid_arg "truncate: n < 3"

type discid_error =
  | Ambiguous of t list
  | Ambiguous_prefix of string * t list
  | Invalid_prefix of string * t list
  | No_Backreference of Mb_release.t list
  | MB_Error of string

let ambiguous_discid discid discs =
  let b = Buffer.create 16 in
  let pr = Printf.bprintf in
  pr b "%d released discs for discid '%s':" (List.length discs) discid;
  pr b "\n  %-36s %-36s" "MEDIUM ID/TITLE" "RELEASE ID/TITLE";
  List.iter
    (fun d ->
      pr b "\n/ %-36s %-36s \\" d.medium.Mb_medium.id d.release.Mb_release.id;
      pr b "\n\\ %-36s %-36s /"
        (truncate 36 (Option.value ~default:"???" d.medium.Mb_medium.title))
        (truncate 36 (Option.value ~default:"???" d.release.Mb_release.title)))
    discs;
  Buffer.contents b

let disambiguate_medium prefix discid discs =
  match List.filter (fun d -> String.starts_with ~prefix d.medium.Mb_medium.id) discs with
  | [disc] -> Ok disc
  | [] -> Error (Printf.sprintf
                   "%s\nno match for medium '%s'"
                   (ambiguous_discid discid discs) prefix)
  | _ -> Error (Printf.sprintf
                  "%s\nmultiple matches for medium '%s'"
                  (ambiguous_discid discid discs) prefix)

let of_discid_sans_lifespans ?medium ~root discid =
  let* disc, other_discs = discs_of_discid ~root discid in
  match other_discs with
  | [] -> Ok disc
  | _ ->
     let discs = disc :: other_discs in
     begin match medium with
     | None -> Error (ambiguous_discid discid discs)
     | Some prefix -> disambiguate_medium prefix discid discs
     end

let of_discid ?medium ~root discid =
  let* disc = of_discid_sans_lifespans ?medium ~root discid in
  add_lifespans ~root disc

let of_discid_local ?medium ~root discid =
  let* disc = of_discid_sans_lifespans ?medium ~root discid in
  add_lifespans_local ~root disc

let discid_error_to_string discid = function
  | Ambiguous releases ->
     ambiguous_discid discid releases
  | MB_Error msg ->
     Printf.sprintf "MusicBrainz returned error '%s' for discid '%s'" msg discid
  | Ambiguous_prefix (prefix, releases) ->
     Printf.sprintf "muliple matches for prefix '%s' in\n%s" prefix (ambiguous_discid discid releases)
  | Invalid_prefix (prefix, releases) ->
     Printf.sprintf "no match for prefix '%s' in\n%s" prefix (ambiguous_discid discid releases)
  | No_Backreference releases ->
     let releases = List.map (fun r -> r.Mb_release.id) releases |> String.concat ", " in
     Printf.sprintf "no disc for discid '%s' in releases %s" discid releases

let print disc =
  let open Printf in
  printf "Disc:    %s\n" disc.discid;
  printf "Medium:  %s\n" disc.medium.Mb_medium.id;
  printf "Release: %s\n" disc.release.Mb_release.id;
  printf "Title:   %s\n" (Option.value disc.release.Mb_release.title ~default:"(no title)");
  begin match disc.release.Mb_release.artist_credits with
  | [] -> ()
  | c :: clist ->
     printf "Artists: %s\n" (Mb_artist_credit.to_string c);
     List.iter (fun c -> printf "         %s\n" (Mb_artist_credit.to_string c)) clist
  end;
  Mb_medium.print disc.medium

