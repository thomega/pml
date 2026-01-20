type t =
  | Alive of Date.t
  | Dead of Date.t * Date.t
  | Dead' of Date.t
  | Limbo

let to_string = function
  | Alive born -> "*" ^ Date.year_to_string born
  | Dead (born, died) -> "*" ^ Date.year_to_string born ^ ", +" ^ Date.year_to_string died
  | Dead' died -> "+" ^ Date.year_to_string died
  | Limbo -> "*/+?"

type relation =
  | Before
  | After
  | Overlap

let relation ls1 ls2 =
  let open Date.Syntax in
  match ls1, ls2 with
  | Dead (b1, d1), Dead (b2, d2) ->
     if d1 <= b2 then
       Before
     else if d2 <= b1 then
       After
     else
       Overlap
  | (Dead (_, d1) | Dead' d1), Alive b2 ->
     if d1 <= b2 then
       Before
     else
       Overlap
  | Alive b1, (Dead (_, d2) | Dead' d2) ->
     if d2 <= b1 then
       After
     else
       Overlap
  | Dead' d1, Dead (b2, _) ->
     if d1 <= b2 then
       Before
     else
       Overlap
  | Dead (b1, _), Dead' d2 ->
     if d2 <= b1 then
       After
     else
       Overlap
  | Limbo, _ | _, Limbo | Alive _, Alive _ | Dead' _, Dead' _ -> Overlap

let compare ls1 ls2 =
  match relation ls1 ls2 with
  | Before -> -1
  | After -> 1
  | Overlap -> 0

let not_performer ?(cutoff=1910) lifespan =
  match relation lifespan (Alive (Date.of_year cutoff)) with
  | Before -> true
  | After | Overlap -> false

(* We have to deal with both optional and nullable strings. *)
let make first last =
  match Date.of_opt_string_opt (Option.join first),
        Date.of_opt_string_opt (Option.join last) with
  | Some born, Some died -> Dead (born, died)
  | Some born, None -> Alive born
  | None, Some died -> Dead' died
  | None, None -> Limbo

let jsont =
  Jsont.Object.map ~kind:"Lifespan" make
  |> Jsont.Object.opt_mem "begin" Jsont.(option string)
  |> Jsont.Object.opt_mem "end" Jsont.(option string)
  |> Jsont.Object.finish
