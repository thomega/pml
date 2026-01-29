open Result.Syntax

module Release =
  struct

    type t =
      { id : string (** While this is optional in the DTD, it should be there anyway. *);
        title : string option;
        artist_credits : Mb_artist_credit.t list;
        media : Mb_medium.t list }

    let make id title artist_credits media =
      let title = Edit.blank_to_none title
      and artist_credits = Option.value ~default:[] artist_credits
      and media = Option.value ~default:[] media in
      { id; title; artist_credits; media }

    let jsont =
      Jsont.Object.map ~kind:"Release" make
      |> Jsont.Object.mem "id" Jsont.string
      |> Jsont.Object.opt_mem "title" Jsont.string
      |> Jsont.Object.opt_mem "artist-credit" Jsont.(list Mb_artist_credit.jsont)
      |> Jsont.Object.opt_mem "media" Jsont.(list Mb_medium.jsont)
      |> Jsont.Object.finish

    let artist_ids r =
      Sets.MBID.union
        (List.map Mb_artist_credit.artist_id r.artist_credits |> Sets.mbid_union)
        (List.map Mb_medium.artist_ids r.media |> Sets.mbid_union)

    let update_artists map r =
      let* artist_credits =
        Result_list.map (Mb_artist_credit.update_artist map) r.artist_credits
      and* media = Result_list.map (Mb_medium.update_artists map) r.media in
      Ok { r with artist_credits; media }

  end

module Taggable =
  struct

    type t =
      { medium : Mb_medium.t;
        release : Release.t;
        discid : string }

    let release_of_mbid ~root mbid =
      let* text = Cached.Release.get ~root mbid in
      Jsont_bytesrw.decode_string Release.jsont text

    let contains_discid discid medium =
      List.exists (fun disc -> discid = disc.Mb_disc.id) medium.Mb_medium.discs

    let discs_of_discid ~root discid =
      let* releases = Cached.releases_of_discid ~root discid in
      let* discs =
        Result_list.map
          (fun mbid ->
            let* release = release_of_mbid ~root mbid in
            let media = List.filter (contains_discid discid) release.Release.media in
            Ok (List.map (fun medium -> { medium; release; discid }) media))
          releases in
      Ok (List.concat discs)

    let artist_ids d =
      Sets.MBID.union
        (Mb_medium.artist_ids d.medium)
        (Release.artist_ids d.release)

    let update_artists map d =
      let* medium = Mb_medium.update_artists map d.medium
      and* release = Release.update_artists map d.release in
      Ok { d with medium; release }

    let add_lifespans ~root disc =
      let ids = artist_ids disc |> Sets.MBID.elements in
      let* artist_map =
        Cached.Artist.map_of_ids ~root (Jsont_bytesrw.decode_string Mb_artist.jsont) ids in
      update_artists artist_map disc

    let truncate n s =
      let l = String.length s in
      if l <= n then
        s
      else if n >= 3 then
        String.sub s 0 (n - 3) ^ "..."
      else
        invalid_arg "truncate: n < 3"

    let ambiguous_discid discid discs =
      let b = Buffer.create 16 in
      let pr = Printf.bprintf in
      pr b "%d released discs for discid '%s':" (List.length discs) discid;
      pr b "\n  %-36s %-36s" "MEDIUM" "RELEASE";
      List.iter
        (fun d ->
          pr b "\n/ %-36s %-36s \\" d.medium.Mb_medium.id d.release.Release.id;
          pr b "\n\\ %-36s %-36s /"
            (truncate 36 (Option.value ~default:"???" d.medium.Mb_medium.title))
            (truncate 36 (Option.value ~default:"???" d.release.Release.title)))
        discs;
      Buffer.contents b

    let disambiguate_medium prefix discid discs =
      ignore discid;
      match List.filter (fun d -> String.starts_with ~prefix d.medium.Mb_medium.id) discs with
      | [disk] -> Ok disk
      | [] -> Error (Printf.sprintf
                       "%s\nno match for medium '%s'"
                       (ambiguous_discid discid discs) prefix)
      | _ -> Error (Printf.sprintf
                      "%s\nmultiple matches for medium '%s'"
                      (ambiguous_discid discid discs) prefix)

    let of_discid_sans_lifespans ?medium ~root discid =
      ignore medium;
      let* discs = discs_of_discid ~root discid in
      match discs with
      | [disc] -> Ok disc
      | [] -> Error (Printf.sprintf "no released disc for discid '%s'" discid)
      | _ ->
         begin match medium with
         | None -> Error (ambiguous_discid discid discs)
         | Some prefix -> disambiguate_medium prefix discid discs
         end

    let of_discid ?medium ~root discid =
      let* disc = of_discid_sans_lifespans ?medium ~root discid in
      add_lifespans ~root disc
      
    let print disc =
      let open Printf in
      printf "Discid: %s\n" disc.discid;
      printf "Release: %s\n" (Option.value disc.release.Release.title ~default:"(no title)");
      begin match disc.release.Release.artist_credits with
      | [] -> ()
      | c :: clist ->
         printf "Artists: %s\n" (Mb_artist_credit.to_string c);
         List.iter (fun c -> printf "         %s\n" (Mb_artist_credit.to_string c)) clist
      end;
      Mb_medium.print disc.medium

  end
