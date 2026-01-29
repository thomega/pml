type t =
  { id : string (** While this is optional in the DTD, it should be there anyway. *);
    title : string option;
    artist_credits : Mb_artist_credit.t list }

let make id title artist_credits =
  let title = Edit.blank_to_none title
  and artist_credits = Option.value ~default:[] artist_credits in
  { id; title; artist_credits }

let jsont =
  Jsont.Object.map ~kind:"Recording" make
  |> Jsont.Object.mem "id" Jsont.string
  |> Jsont.Object.opt_mem "title" Jsont.string
  |> Jsont.Object.opt_mem "artist-credit" Jsont.(list Mb_artist_credit.jsont)
  |> Jsont.Object.finish

let artist_ids r =
  List.map Mb_artist_credit.artist_id r.artist_credits |> Sets.mbid_union

let update_artists map r =
  let open Result.Syntax in
  let* artist_credits =
    Result_list.map (Mb_artist_credit.update_artist map) r.artist_credits in
  Ok { r with artist_credits }

let print r =
  let open Printf in
  printf "      Rec.: %s\n"
    (match r.title with
     | None | Some "" -> "[" ^ r.id ^ "]"
     | Some s -> s);
  begin match r.artist_credits with
  | [] -> ()
  | c :: clist ->
     printf "      Art.: %s\n" (Mb_artist_credit.to_string c);
     List.iter (fun c -> printf "            %s\n" (Mb_artist_credit.to_string c)) clist
  end;
  ()
