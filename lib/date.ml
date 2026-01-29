(* date.ml -- part of PML (Physical Media Library)

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

type t =
  { year : int;
    month : int option;
    day : int option }

let of_year ?month ?day year =
  { year; day; month }

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
