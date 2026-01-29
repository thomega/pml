type t =
  { id : string (** While this is optional in the DTD, it should be there anyway. *);
    name : string option;
    sort_name : string option;
    artist_type : Artist_type.t option;
    lifespan : Lifespan.t option;
    disambiguation : string option }

let make id name sort_name artist_type lifespan disambiguation =
  let name = Edit.blank_to_none name
  and sort_name = Edit.blank_to_none sort_name in
  let roles =
    match disambiguation with
    | None -> Artist_type.no_role
    | Some disambiguation -> Artist_type.roles_of_string disambiguation in
  let artist_type = Option.map (Artist_type.of_string roles) artist_type in
  { id; name; sort_name; artist_type; lifespan; disambiguation }

let jsont =
  Jsont.Object.map ~kind:"Artist" make
  |> Jsont.Object.mem "id" Jsont.string
  |> Jsont.Object.opt_mem "name" Jsont.string
  |> Jsont.Object.opt_mem "sort-name" Jsont.string
  |> Jsont.Object.opt_mem "type" Jsont.string
  |> Jsont.Object.opt_mem "life-span" Lifespan.jsont
  |> Jsont.Object.opt_mem "disambiguation" Jsont.string
  |> Jsont.Object.finish

let id a = Sets.MBID.singleton a.id

let update map a =
  (* List.iter (fun (s, _) -> Printf.printf "key: %s\n" s) (Cached.Artist.M.bindings map); *)
  match Cached.Artist.M.find_opt a.id map with
  | Some a -> Ok a
  | None -> Error (Printf.sprintf "Artist ID '%s' not found!" a.id)

let compare a1 a2 =
  let c =
    match a1.artist_type, a2.artist_type with
    | Some at1, Some at2 -> Artist_type.compare at1 at2
    | Some _, None -> -1
    | None, Some _ -> 1
    | None, None -> 0 in
  if c <> 0 then
    c
  else
    let c = 
      match a1.lifespan, a2.lifespan with
      | Some ls1, Some ls2 -> Lifespan.compare ls1 ls2
      | Some _, None -> -1
      | None, Some _ -> 1
      | None, None -> 0 in
    if c <> 0 then
      c
    else
      let c =
        match a1.sort_name, a2.sort_name with
        | Some n1, Some n2 -> String.compare n1 n2
        | Some _, None -> -1
        | None, Some _ -> 1
        | None, None -> 0 in
      if c <> 0 then
        c
      else
        String.compare a1.id a2.id

    let to_string a =
      (Option.value a.sort_name ~default:(Option.value a.name ~default:"(anonymous)"))
      ^ (match a.disambiguation with
         | None -> ""
         | Some s -> " (" ^ s) ^ ")"
      ^ (Option.fold ~none:"" ~some:(fun t -> " {" ^ Artist_type.to_string t ^ "}") a.artist_type)
      ^ (Option.fold ~none:"" ~some:(fun ls -> " [" ^ Lifespan.to_string ls ^ "]") a.lifespan)
