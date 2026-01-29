(* artist_type.ml -- part of PML (Physical Media Library)

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

type voice =
  | Soprano
  | Mezzo
  | Alto
  | Counter
  | Tenor
  | Bariton
  | Bass

let voice_to_rank_and_string = function
  | Soprano -> (1, "S.")
  | Mezzo -> (2, "Mez.")
  | Alto -> (3, "A.")
  | Counter -> (4, "Ct.")
  | Tenor -> (5, "T.")
  | Bariton -> (6, "Bar.")
  | Bass -> (7, "B.")

type instrument =
  | Piano
  | Violin
  | Viola
  | Guitar

let instrument_to_rank_and_string = function
  | Piano -> (1, "p.")
  | Violin -> (2, "vln.")
  | Viola -> (3, "vla.")
  | Guitar -> (4, "gtr.")

type role =
  | Composer
  | Conductor
  | Singer of voice
  | Player of instrument

let role_to_rank_and_string = function
  | Composer -> ([1], "comp.")
  | Conductor -> ([2], "cond.")
  | Singer voice ->
     let r, s = voice_to_rank_and_string voice in
     ([3; r], s)
  | Player instrument ->
     let r, s = instrument_to_rank_and_string instrument in
     ([4; r], s)

let role_to_rank r =
  role_to_rank_and_string r |> fst

let role_to_string r =
  role_to_rank_and_string r |> snd

let compare_roles r1 r2 =
  List.compare Int.compare (role_to_rank r1) (role_to_rank r2)

module Roles = Set.Make (struct type t = role let compare = compare_roles end)

let no_role = Roles.empty

let roles_to_string roles =
  Roles.elements roles
  |> List.map role_to_string
  |> String.concat "/"

let string_role_alist =
  [("composer", Composer);
   ("conductor", Conductor);
   ("soprano", Singer Soprano);
   ("mezzo", Singer Mezzo);
   ("alto", Singer Alto);
   ("counter tenor", Singer Counter);
   ("tenor", Singer Tenor);
   ("baritone", Singer Bariton);
   ("bass", Singer Bass);
   ("pianist", Player Piano);
   ("violinist", Player Violin);
   ("viola player", Player Viola);
   ("guitarist", Player Guitar)]

let re_word s =
  Re.(seq [bow; str s; eow] |> no_case |> compile)

let re_role_alist =
  List.map (fun (s, r) -> (re_word s, r)) string_role_alist
          
let roles_of_string s =
  List.fold_left
    (fun set (re, role) ->
      if Re.execp re s then
        Roles.add role set
      else
        set)
    no_role re_role_alist

let%test_module _ =
  (module struct

     let normalize s_in s_out =
       roles_of_string s_in |> roles_to_string = s_out

     let%test _ = normalize "" ""
     let%test _ = normalize "composer and soprano" "comp./S."
     let%test _ = normalize "baritone and pianist" "Bar./p."
     let%test _ = normalize "violinist, conductor, tenor, composer" "comp./cond./T./vln."

   end)

type t =
  | Person of Roles.t
  | Group
  | Orchestra
  | Choir
  | Character
  | Other
  | Unknown of string

let of_string roles = function
  | "Person" -> Person roles
  | "Group" -> Group
  | "Orchestra" -> Orchestra
  | "Choir" -> Choir
  | "Character" -> Character
  | "Other" -> Other
  | s -> Unknown s

let to_rank_and_string = function
  | Person roles ->
     begin match Roles.min_elt_opt roles with
     | None -> ([1; 5], "Person")
     | Some role ->
        let r, _ = role_to_rank_and_string role in
        (1 :: r, roles_to_string roles)
     end
  | Group -> ([2], "Group")
  | Orchestra -> ([3], "Orchestra")
  | Choir -> ([4], "Choir")
  | Character -> ([5], "Character")
  | Other -> ([6], "Other")
  | Unknown s -> ([7], "?" ^ s ^ "?")

let to_rank artist_type =
  to_rank_and_string artist_type |> fst

let to_string artist_type =
  to_rank_and_string artist_type |> snd

let compare t1 t2 =
  List.compare Int.compare (to_rank t1) (to_rank t2)

let has_role role = function
  | Person roles -> Roles.mem role roles
  | _ -> false

let is_composer = has_role Composer
let is_conductor = has_role Conductor

