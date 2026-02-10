(* sets.ml -- part of PML (Physical Media Library)

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

module MBID = Set.Make (String)

let mbid_union sets =
  List.fold_left MBID.union MBID.empty sets

module Integers =
  struct

    module S = Set.Make (Int)

    type range = { first : int; last : int }

    let of_range { first; last } =
      if first > last then
        S.empty
      else
        let rec add set i =
          if i > last then
            set
          else
            add (S.add i set) (succ i) in
        add (S.singleton first) (succ first)

    let of_ranges ranges =
      List.map of_range ranges |> List.fold_left S.union S.empty

    let%test _ = of_ranges [{first = 5; last = 4 }] |> S.elements = []
    let%test _ = of_ranges [{first = 4; last = 4 }] |> S.elements = [4]
    let%test _ = of_ranges [{first = 4; last = 7 }] |> S.elements = [4;5;6;7]
    let%test _ = of_ranges [{first = 4; last = 7 }; {first = 7; last = 8} ] |> S.elements = [4;5;6;7;8]

    let parse_int s =
      try
        Ok (int_of_string s)
      with
      | e -> Error (Printexc.to_string e)

    let range_of_string s =
      let open Result.Syntax in
      match String.split_on_char '-' s with
      | [] -> assert false
      | [""] -> Ok { first = 0; last = -1 }
      | [first] ->
         let* first = parse_int first in
         Ok { first; last = first }
      | [first;last] ->
         let* first = parse_int first
         and* last = parse_int last in
         Ok { first; last }
      | _ -> Error "too many '-'s in integer range"

    let ranges_of_string s =
      String.split_on_char ',' s |> Result_list.map range_of_string

    let of_string s =
      let open Result.Syntax in
      let* ranges = ranges_of_string s in
      Ok (of_ranges ranges)
      
    let%test_module _ =
      (module struct

         let expect s ilist =
           let open Result.Syntax in
           match
             let* set = of_string s in
             Ok (S.elements set = List.sort Int.compare ilist)
           with
           | Error msg -> prerr_endline msg; false
           | Ok tof -> tof

         let%test _ = expect "" []
         let%test _ = expect "13" [13]
         let%test _ = expect "13,18" [13; 18]
         let%test _ = expect "13-18" [13; 14; 15; 16; 17; 18]
         let%test _ = expect "18-13" []
         let%test _ = expect "9,13-15,7" [7; 9; 13; 14; 15]
         let%test _ = expect "7-9,13-15" [7; 8; 9; 13; 14; 15]
         let%test _ = expect "7-9,8-10" [7; 8; 9; 10]

       end)

  end
