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
  let module T = Artist_type in
  let id = mb.Mb_artist.id
  and name =
    match mb.Mb_artist.sort_name, mb.Mb_artist.name with
    | Some sort_name, Some _name -> sort_name
    | Some sort_name, None -> sort_name
    | None, Some name -> sort_name_of_name name
    | None, None -> "(anonymous)"
  and artist_type =
    Option.value mb.Mb_artist.artist_type ~default:(T.Person T.Roles.empty)
  and lifespan =
    Option.value mb.Mb_artist.lifespan ~default:Lifespan.Limbo in
  { id; name; artist_type; lifespan }

let of_name name =
  let artist_type = Artist_type.(Person Roles.empty)
  and lifespan = Lifespan.Limbo
  and id = "" in
  {name; artist_type; lifespan; id }

type artist_t = t
(** We can't write [type t = t] below. *)

module Collection = Set.Make (struct type t = artist_t let compare = compare end)

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
  Collection.elements artists
  |> List.sort (fun a1 a2 -> Lifespan.compare a1.lifespan a2.lifespan)
  |> find_gaps
       (fun a1 a2 ->
         match Lifespan.relation a1.lifespan a2.lifespan with
         | Before -> true
         | After | Overlap -> false)
  |> List.map Collection.of_list

let of_credits credits =
  List.filter_map (fun c -> Option.map of_mb c.Mb_artist_credit.artist) credits
  |> Collection.of_list

