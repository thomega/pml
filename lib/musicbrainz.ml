open Result.Syntax

module type Table =
  sig

    val valid_key : string -> (string, string) result
    (** Check validity of the key, to avoid unnecessary
        lookups of invalid key. *)

    val query : Query.query
    (** What to ask Musicbrainz. *)

  end

module Discid_table : Table =
  struct

    (* A sequence of exactly 28 characters from the set [A-Za-z0-9._-]. *)

    (* [Re.alnum] contains accented characters! *)

    let alphanum =
      Re.(alt [rg 'A' 'Z'; rg 'a' 'z'; rg '0' '9'])

    (* NB: '-' can appear only as padding at the end.  We must check
       the length of the strong as well. *)

    let re_discid =
      Re.(seq [start; alt [alphanum; set "._"] |> rep1; set "-" |> rep; stop] |> compile)

    let _is_discid s =
      String.length s = 28 && Re.execp re_discid s

    (* NB: The DTD at http://musicbrainz.org/development/mmd/ is stricter:
       exactly one '-' at the end! *)

    let re_discid =
      Re.(seq [start; repn (alt [alphanum; set "._"]) 27 (Some 27); set "-"; stop] |> compile)

    let is_discid s =
      Re.execp re_discid s

    let valid_key discid =
      if is_discid discid then
        Ok discid
      else
        Error (Printf.sprintf "'%s' is not a valid discid" discid)

    let query = Query.{ table = "discid"; inc = [] }

  end

(* A MBID/UUID: 8-4-4-4-12 hex digits [0-9a-fA-F] *)

let hexrep n =
  Re.(repn xdigit n (Some n))

let re_uuid =
  Re.(seq [start;
           hexrep 8; set "-";
           hexrep 4; set "-";
           hexrep 4; set "-";
           hexrep 4; set "-";
           hexrep 12;
           stop]
      |> compile)

let is_uuid s =
  Re.execp re_uuid s

let valid_mbid mbid =
  if is_uuid mbid then
    Ok (String.lowercase_ascii mbid)
  else
    Error (Printf.sprintf "'%s' is not a valid MBID" mbid)

module Release_table : Table =
  struct

    let valid_key = valid_mbid

    let query =
      Query.{ table = "release";
              inc = ["artists"; "artist-credits"; "aliases";
                     "recordings"; "release-groups"; "discids";
                     "url-rels"; "labels"; ] }

  end

module Artist_table : Table =
  struct

    let valid_key = valid_mbid

    let query =
      Query.{ table = "artist";
              inc = ["aliases"] }

  end

module type Cached =
  sig
    val get : root:string -> string -> (string, string) result
    val local : root:string -> string -> (string option, string) result
    val remote : string -> (string, string) result
    val all_local : root:string -> ((string * string) list, string) result
    val url : string -> (string, string) result
    module M : Map.S with type key = string
    val map_of_ids : root:string -> (string -> ('a, string) result) ->
                     string list -> ('a M.t, string) result
    module Internal : Cache.T with type key = string and type value = string
  end

module Cached (Table : Table) : Cached =
  struct

    module C =
      Cache.Make
        (struct
          let name = Table.query.table
          type key = string
          let key_of_string = Table.valid_key
          let key_to_string = Table.valid_key
          type value = string
          let value_of_string = Result.ok
          let value_to_string = Result.ok
        end)

    (* This version deosn't check its argument.
       It can be used in [get] below,
       because the argument has been checked. *)
    let remote_unsafe key =
      let* text = Query.(exec musicbrainz Table.query key) in
      Mb_raw.normalize text

    let remote key =
      let* key = Table.valid_key key in
      remote_unsafe key

    let local = C.get

    let get ~root key =
      let* text = C.lookup ~root key remote_unsafe in
      match Mb_error.get_error_opt text with
      | Some msg -> Error (Printf.sprintf "get '%s' returns error object: %s" key msg)
      | None -> Ok text

    let url key =
      let* key = Table.valid_key key in
      Ok (Query.(url musicbrainz Table.query key))

    let all_local = C.to_alist

    module M = Map.Make (String)

    let map_of_ids ~root value_of_string keys =
      Result_list.fold_left
        (fun map key ->
          let* text = get ~root key in
          let* value = value_of_string text in
          Ok (M.add key value map))
        M.empty keys

    module Internal = C
  end

module Discid_cached = Cached (Discid_table)

let opt_list = function
  | Some l -> l
  | None -> []

module Release_Short =
  struct
    type t =
      { id : string }
    let make id =
      { id }
    let jsont =
      Jsont.Object.map ~kind:"Release_Short" make
      |> Jsont.Object.mem "id" Jsont.string
      |> Jsont.Object.finish
  end

module Discid =
  struct
    type t =
      { id : string;
        releases : Release_Short.t list }
    let make id releases =
      let releases = opt_list releases in
      { id; releases }
    let jsont =
      Jsont.Object.map ~kind:"Discid" make
      |> Jsont.Object.mem "id" Jsont.string
      |> Jsont.Object.opt_mem "releases" Jsont.(list Release_Short.jsont)
      |> Jsont.Object.finish

  end

let releases_of_discid ~root discid =
  let* json = Discid_cached.get ~root discid in
  let* discid' = Jsont_bytesrw.decode_string Discid.jsont json in
  if discid'.Discid.id <> discid then
    Error (Printf.sprintf "inconsistent discid: '%s' <> '%s'" discid'.Discid.id discid)
  else
    match discid'.Discid.releases with
    | [] -> Error "No Releases"
    | releases -> Ok (List.map (fun r -> r.Release_Short.id) releases)

module Release_cached = Cached (Release_table)

module Artist_cached = Cached (Artist_table)

module MBID_Set = Set.Make (String)

let mbid_union sets =
  List.fold_left MBID_Set.union MBID_Set.empty sets

let re_blank =
  Re.(seq [start; rep blank; stop] |> compile)

let is_blank s =
  Re.execp re_blank s

let blank_to_none = function
  | None | Some "" -> None
  | Some s as string_option -> if is_blank s then None else string_option

module Artist =
  struct

    type t =
      { id : string (** While this is optional in the DTD, it should be there anyway. *);
        name : string option;
        sort_name : string option;
        artist_type : Artist_type.t option;
        lifespan : Lifespan.t option;
        disambiguation : string option }

    let make id name sort_name artist_type lifespan disambiguation =
      let name = blank_to_none name
      and sort_name = blank_to_none sort_name in
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

    let id a = MBID_Set.singleton a.id

    let update map a =
      (* List.iter (fun (s, _) -> Printf.printf "key: %s\n" s) (Artist_cached.M.bindings map); *)
      match Artist_cached.M.find_opt a.id map with
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

  end

module Artist_Credit =
  struct

    type t =
      { name : string option;
        artist : Artist.t option }

    let make name artist =
      let name = blank_to_none name in
      { name; artist }

    let jsont =
      Jsont.Object.map ~kind:"Artist_Credit" make
      |> Jsont.Object.opt_mem "name" Jsont.string
      |> Jsont.Object.opt_mem "artist" Artist.jsont
      |> Jsont.Object.finish

    let artist_id c =
      match c.artist with
      | None -> MBID_Set.empty
      | Some artist -> Artist.id artist
      
    let update_artist map c =
      match c.artist with
      | Some artist ->
         let* artist = Artist.update map artist in
         Ok { c with artist = Some artist }
      | None -> Ok c

    let to_string c =
      match c.artist with
      | Some artist -> Artist.to_string artist
      | None -> Option.value c.name ~default:"(anonymous)"

  end

module Recording =
  struct

    type t =
      { id : string (** While this is optional in the DTD, it should be there anyway. *);
        title : string option;
        artist_credits : Artist_Credit.t list }

    let make id title artist_credits =
      let title = blank_to_none title
      and artist_credits = opt_list artist_credits in
      { id; title; artist_credits }

    let jsont =
      Jsont.Object.map ~kind:"Recording" make
      |> Jsont.Object.mem "id" Jsont.string
      |> Jsont.Object.opt_mem "title" Jsont.string
      |> Jsont.Object.opt_mem "artist-credit" Jsont.(list Artist_Credit.jsont)
      |> Jsont.Object.finish

    let artist_ids r =
      List.map Artist_Credit.artist_id r.artist_credits |> mbid_union

    let update_artists map r =
      let* artist_credits =
        Result_list.map (Artist_Credit.update_artist map) r.artist_credits in
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
         printf "      Art.: %s\n" (Artist_Credit.to_string c);
         List.iter (fun c -> printf "            %s\n" (Artist_Credit.to_string c)) clist
      end;
      ()

  end

module Track =
  struct

    type t =
      { id : string (** While this is optional in the DTD, it should be there anyway. *);
        position : int option;
        title : string option;
        artist_credits : Artist_Credit.t list;
        recording : Recording.t option }

    let make id position title artist_credits recording =
      let title = blank_to_none title
      and artist_credits = opt_list artist_credits in
      { id; position; title; artist_credits; recording }

    let jsont =
      Jsont.Object.map ~kind:"Track" make
      |> Jsont.Object.mem "id" Jsont.string
      |> Jsont.Object.opt_mem "position" Jsont.int
      |> Jsont.Object.opt_mem "title" Jsont.string
      |> Jsont.Object.opt_mem "artist-credit" Jsont.(list Artist_Credit.jsont)
      |> Jsont.Object.opt_mem "recording" Recording.jsont
      |> Jsont.Object.finish

    let artist_ids t =
      let artist_credits =
        List.map Artist_Credit.artist_id t.artist_credits |> mbid_union in
      match t.recording with
      | None -> artist_credits
      | Some recording -> MBID_Set.union (Recording.artist_ids recording) artist_credits

    let print n t =
      let open Printf in
      printf "  Trk%2d.%02d: %s\n"
        n
        (Option.value t.position ~default:0)
        (match t.title with
         | None | Some "" -> "[" ^ t.id ^ "]"
         | Some s -> s);
      begin match t.artist_credits with
      | [] -> ()
      | c :: clist ->
         printf "      Art.: %s\n" (Artist_Credit.to_string c);
         List.iter (fun c -> printf "            %s\n" (Artist_Credit.to_string c)) clist
      end;
      begin match t.recording with
      | None -> ()
      | Some r -> Recording.print r
      end;
      ()

    let update_artists map t =
      let* artist_credits =
        Result_list.map (Artist_Credit.update_artist map) t.artist_credits in
      match t.recording with
      | Some recording ->
         let* recording = Recording.update_artists map recording in
         Ok { t with artist_credits; recording = Some recording }
      | None ->
         Ok { t with artist_credits }

  end

module Disc =
  struct
    type t =
      { id : string (** While this is optional in the DTD, it should be there anyway. *);
      }
    let make id =
      { id }
    let jsont =
      Jsont.Object.map ~kind:"Disc" make
      |> Jsont.Object.mem "id" Jsont.string
      |> Jsont.Object.finish
  end

module Medium =
  struct
    type t =

      { id : string (** While this is optional in the DTD, it should be there anyway. *);
        position : int option;
        title : string option;
        discs : Disc.t list;
        tracks : Track.t list }

    let make id position title discs tracks =
      let discs = opt_list discs
      and tracks = opt_list tracks
      and title = blank_to_none title in
      { id; position; title; discs; tracks }

    let jsont =
      Jsont.Object.map ~kind:"Medium" make
      |> Jsont.Object.mem "id" Jsont.string
      |> Jsont.Object.opt_mem "position" Jsont.int
      |> Jsont.Object.opt_mem "title" Jsont.string
      |> Jsont.Object.opt_mem "discs" Jsont.(list Disc.jsont)
      |> Jsont.Object.opt_mem "tracks" Jsont.(list Track.jsont)
      |> Jsont.Object.finish

    let artist_ids m =
      List.map Track.artist_ids m.tracks |> mbid_union

    let update_artists map m =
      let* tracks =
        Result_list.map (Track.update_artists map) m.tracks in
      Ok { m with tracks }

    let print m =
      let open Printf in
      let n = Option.value m.position ~default:0 in
      printf "Disc %2d: %s\n"
        n
        (match m.title with
         | None | Some "" -> "[" ^ m.id ^ "]"
         | Some s -> s);
      List.iter (Track.print n) m.tracks;
      ()

  end

module Release =
  struct

    type t =
      { id : string (** While this is optional in the DTD, it should be there anyway. *);
        title : string option;
        artist_credits : Artist_Credit.t list;
        media : Medium.t list }

    let make id title artist_credits media =
      let title = blank_to_none title
      and artist_credits = opt_list artist_credits
      and media = opt_list media in
      { id; title; artist_credits; media }

    let jsont =
      Jsont.Object.map ~kind:"Release" make
      |> Jsont.Object.mem "id" Jsont.string
      |> Jsont.Object.opt_mem "title" Jsont.string
      |> Jsont.Object.opt_mem "artist-credit" Jsont.(list Artist_Credit.jsont)
      |> Jsont.Object.opt_mem "media" Jsont.(list Medium.jsont)
      |> Jsont.Object.finish

    let artist_ids r =
      MBID_Set.union
        (List.map Artist_Credit.artist_id r.artist_credits |> mbid_union)
        (List.map Medium.artist_ids r.media |> mbid_union)

    let update_artists map r =
      let* artist_credits =
        Result_list.map (Artist_Credit.update_artist map) r.artist_credits
      and* media = Result_list.map (Medium.update_artists map) r.media in
      Ok { r with artist_credits; media }

  end

module Taggable =
  struct

    type t =
      { medium : Medium.t;
        release : Release.t;
        discid : string }

    let release_of_mbid ~root mbid =
      let* text = Release_cached.get ~root mbid in
      Jsont_bytesrw.decode_string Release.jsont text

    let contains_discid discid medium =
      List.exists (fun disc -> discid = disc.Disc.id) medium.Medium.discs

    let discs_of_discid ~root discid =
      let* releases = releases_of_discid ~root discid in
      let* discs =
        Result_list.map
          (fun mbid ->
            let* release = release_of_mbid ~root mbid in
            let media = List.filter (contains_discid discid) release.Release.media in
            Ok (List.map (fun medium -> { medium; release; discid }) media))
          releases in
      Ok (List.concat discs)

    let artist_ids d =
      MBID_Set.union
        (Medium.artist_ids d.medium)
        (Release.artist_ids d.release)

    let update_artists map d =
      let* medium = Medium.update_artists map d.medium
      and* release = Release.update_artists map d.release in
      Ok { d with medium; release }

    let add_lifespans ~root disc =
      let ids = artist_ids disc |> MBID_Set.elements in
      let* artist_map =
        Artist_cached.map_of_ids ~root (Jsont_bytesrw.decode_string Artist.jsont) ids in
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
          pr b "\n/ %-36s %-36s \\" d.medium.Medium.id d.release.Release.id;
          pr b "\n\\ %-36s %-36s /"
            (truncate 36 (Option.value ~default:"???" d.medium.Medium.title))
            (truncate 36 (Option.value ~default:"???" d.release.Release.title)))
        discs;
      Buffer.contents b

    let disambiguate_medium prefix discid discs =
      ignore discid;
      match List.filter (fun d -> String.starts_with ~prefix d.medium.Medium.id) discs with
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
         printf "Artists: %s\n" (Artist_Credit.to_string c);
         List.iter (fun c -> printf "         %s\n" (Artist_Credit.to_string c)) clist
      end;
      Medium.print disc.medium

  end
