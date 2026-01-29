(* result_list.mli -- part of PML (Physical Media Library)

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

(** Maps, folds and filters for [Result.t]. *)

val cons : 'a -> ('a list, 'e) result -> ('a list, 'e) result
val cons' : ('a, 'e) result -> ('a list, 'e) result -> ('a list, 'e) result
val hd : 'e -> ('a list, 'e) result -> ('a, 'e) result
val tl : 'e -> ('a list, 'e) result -> ('a list, 'e) result

val map : ('a -> ('b, 'e) result) -> 'a list -> ('b list, 'e) result
(** Like [List.map] but with early exit error handling.
    The returned error will be the one of the {e first}
    element in the list causing an error.
    {e Not} tail recursive and not even tail-mod-cons,
    because we must inspect the results. *)

(** Note that a 
    [val map' : ('a -> ('b, 'e) result) -> ('a list, 'e) result -> ('b list, 'e) result]
    would just be syntactic sugar for
    {v
     let map' f alist =
       let* alist in
       map f alist
     v} *)

val fold_left : ('a -> 'b -> ('a, 'e) result) -> 'a -> 'b list -> ('a, 'e) result
(** Like [List.fold_left] but with error handling.  The returned error will be
    the one of the {e first} element in the list causing an error. *)

val fold_right : ('a -> 'b -> ('b, 'e) result) -> 'a list -> 'b -> ('b, 'e) result
(** Like [List.fold_right] but with error handling.  The returned error will be
    the one of the {e last} element in the list causing an error. *)
