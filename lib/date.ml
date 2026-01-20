type t =
  { year : int;
    month : int option;
    day : int option }

let to_string t =
  match t.month, t.day with
  | None, None -> Printf.sprintf "%04d" t.year
  | Some month, None -> Printf.sprintf "%04d-%02d" t.year month
  | Some month, Some day -> Printf.sprintf "%04d-%02d-%02d" t.year month day
  | None, Some day -> Printf.sprintf "%04d-??-%02d" t.year day

let year_to_string t =
  Printf.sprintf "%4d" t.year

let of_string_opt s =
  match Scanf.sscanf_opt s "%4d-%2d-%2d"
          (fun year month day -> { year; month = Some month; day = Some day }) with
  | Some _ as d -> d
  | None ->
     begin match Scanf.sscanf_opt s "%4d-%2d"
          (fun year month -> { year; month = Some month; day = None }) with
     | Some _ as d -> d
     | None ->
        Scanf.sscanf_opt s "%4d" (fun year -> { year; month = None; day = None })
     end

let of_opt_string_opt s_opt =
  match s_opt with
  | None -> None
  | Some s -> of_string_opt s

let compare_opt o1 o2 =
  match o1, o2 with
  | Some i1, Some i2 -> Int.compare i1 i2
  | None, Some _ -> -1
  | Some _, None -> 1
  | None, None -> 0

let compare d1 d2 =
  let c = Int.compare d1.year d2.year in
  if c <> 0 then
    c
  else
    let c = compare_opt d1.month d2.month in
    if c <> 0 then
      c
    else
      compare_opt d1.day d2.day

module Syntax =
  struct
    let ( = ) d1 d2 = compare d1 d2 = 0
    let ( < ) d1 d2 = compare d1 d2 < 0
    let ( <= ) d1 d2 = compare d1 d2 <= 0
    let ( > ) d1 d2 = compare d1 d2 > 0
    let ( >= ) d1 d2 = compare d1 d2 >= 0
  end
