(* option_syntax.ml -- part of PML (Physical Media Library)

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

let product o1 o2 =
  match o1, o2 with
  | Some a1, Some a2 -> Some (a1, a2)
  | _, _ -> None

let ( let* ) = Option.bind
let ( and* ) = product

let ( let+ ) r f = Option.map f r
let ( and+ ) = product

let%test _ =
  match
    let* a = Some 1
    and* b = Some 2 in
    Some (a + b)
  with
  | Some 3 -> true
  | _ -> false

let%test _ =
  match
    let* a = None
    and* b = Some 2 in
    Some (a + b)
  with
  | None -> true
  | _ -> false

let%test _ =
  match
    let* a = Some 1
    and* b = None in
    Some (a + b)
  with
  | None -> true
  | _ -> false

let%test _ =
  match
    let* a = None
    and* b = None in
    Some (a + b)
  with
  | None -> true
  | _ -> false

let%test _ =
  match
    let* a = Some 1 in
    let* a = Some (a + 2) in
    Some a
  with
  | Some 3 -> true
  | _ -> false

let%test _ =
  match
    let* a = None in
    let* a = Some (a + 2) in
    Some a
  with
  | None -> true
  | _ -> false

let%test _ =
  match
    let* _ = Some 1 in
    let* a = None in
    Some a
  with
  | None -> true
  | _ -> false

let%test _ =
  match
    let* _ = None in
    let* a = None in
    Some a
  with
  | None -> true
  | _ -> false

let%test _ =
  match
    let+ a = Some 1
    and+ b = Some 2 in
    a + b
  with
  | Some 3 -> true
  | _ -> false

let%test _ =
  match
    let+ a = Some 1
    and+ b = None in
    a + b
  with
  | None -> true
  | _ -> false

let%test _ =
  match
    let+ a = None
    and+ b = Some 2 in
    a + b
  with
  | None -> true
  | _ -> false

let%test _ =
  match
    let+ a = None
    and+ b = None in
    a + b
  with
  | None -> true
  | _ -> false

