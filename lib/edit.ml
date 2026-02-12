(* edit.ml -- part of PML (Physical Media Library)

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

let common_prefix' n s1 slist =
  let split_prefix i =
    (String.sub s1 0 i,
     List.map (fun s -> String.sub s i (String.length s - i)) (s1 :: slist)) in
  let rec common_prefix'' i =
    if i >= n then
      split_prefix i
    else
      let c = s1.[i] in
      if List.exists (fun s -> s.[i] <> c) slist then
        split_prefix i
      else
        common_prefix'' (succ i) in
  common_prefix'' 0

let common_prefix = function
  | [] -> ("", [])
  | [s] -> (s, [""])
  | s1 :: slist ->
     let min_len =
       List.fold_left (fun acc s -> min acc (String.length s)) (String.length s1) slist in
     common_prefix' min_len s1 slist

let%test _ = common_prefix [] = ("", [])
let%test _ = common_prefix ["abc"] = ("abc", [""])
let%test _ = common_prefix ["abc"; "abd"] = ("ab", ["c"; "d"])
let%test _ = common_prefix ["abc"; "abd"] = ("ab", ["c"; "d"])
let%test _ = common_prefix ["abc"; "a"; "abd"] = ("a", ["bc"; ""; "bd"])
let%test _ = common_prefix ["abc"; ""; "abd"] = ("", ["abc"; ""; "abd"])

let set_white =
  Re.set " \t\n\r"

let re_blank =
  Re.(seq [start; rep set_white; stop] |> compile)

let is_blank s =
  Re.execp re_blank s

let blank_to_none = function
  | None | Some "" -> None
  | Some s as string_option ->
     if is_blank s then
       None
     else
       string_option

let re_double_quote =
  Re.(set {|"|} |> compile)

let _re_quotes =
  Re.(set {|"'|} |> compile)

(** Compress all whitespace, including newlines, to a single blank. *)
let _re_rep_white =
  Re.(rep1 set_white |> compile)

(** Whitespace at the beginning or end is not illegal, but superfluous. *)
let re_boundary_white =
  Re.(alt [seq [start; rep1 set_white]; seq [rep1 set_white; stop]] |> compile)

(** Concession to Windows: also replace backslashes and colons.*)
let re_slash =
  Re.(set {|/\|} |> compile)

let re_colon =
  Re.(set {|:|} |> compile)

(** Android appears wants us to kill double quotes. *)

(** TODO: Android doesn't like question marks '?'. What's a good replacement? *)
let filename_safe s =
  s
  |> Re.replace_string re_slash ~by:"-"
  |> Re.replace_string re_colon ~by:" -"
  |> Re.replace_string re_double_quote ~by:"'"
  |> Re.replace_string re_boundary_white ~by:""

(** NB: We have removed the comprssion of whitespace:
    [|> Re.replace_string re_rep_white ~by:" "].  *)

let%test _ = filename_safe {|a: b|} = {|a - b|}
let%test _ = filename_safe {|a / b|} = {|a - b|}
let%test _ = filename_safe {| a \ b|} = {|a - b|}

let sq = {|'|}
let dq = {|"|}

let re_single_quote =
  Re.(set sq |> compile)
  
let shell_single_quote s =
  sq ^ Re.replace_string re_single_quote ~by:{|'"'"'|} s ^ sq
  
let%test _ = shell_single_quote {|abc|} = {|'abc'|}
let%test _ = shell_single_quote {|$abc|} = {|'$abc'|}
let%test _ = shell_single_quote {|a'b|} = {|'a'"'"'b'|}
let%test _ = shell_single_quote {|'ab|} = {|''"'"'ab'|}
let%test _ = shell_single_quote {|ab'|} = {|'ab'"'"''|}
let%test _ = shell_single_quote {|'|} = {|''"'"''|}
let%test _ = shell_single_quote {|a''b|} = {|'a'"'"''"'"'b'|}

let re_double_quote_escape =
  Re.(set {|"$`\|} |> compile)
  
let shell_double_quote s =
  dq ^ Re.replace re_double_quote_escape ~f:(fun group -> {|\|} ^ Re.Group.get group 0) s ^ dq

let%test _ = shell_double_quote {|abc|} = {|"abc"|}
let%test _ = shell_double_quote {|$abc|} = {|"\$abc"|}
let%test _ = shell_double_quote {|$a"b`c|} = {|"\$a\"b\`c"|}
let%test _ = shell_double_quote {|\$a""b`c|} = {|"\\\$a\"\"b\`c"|}

type ranges = Sets.Integers.S.t option
type 'a ranged = ranges * 'a

let in_range i = function
  | None -> true
  | Some ranges -> Sets.Integers.S.mem i ranges

let ranges_to_list_opt = Option.map Sets.Integers.S.elements

let ranges_to_string = function
  | None -> ""
  | Some ranges ->
     if Sets.Integers.S.is_empty ranges then
       "1-0"
     else
       Sets.Integers.S.elements ranges |> List.map string_of_int |> String.concat ","

let range =
  Re.(seq [rep1 digit; opt (seq [char '-'; rep1 digit])])

let ranges =
  Re.(seq [range; rep (seq [char ','; range])])

let re_ranged =
  Re.(seq [start;
           group (opt ranges);
           group (seq [compl [digit]; rep any]);
           stop] |> compile)

(** We use [Re.Group.get] instead of [Re.Group.get_opt] because
    there are exactly 2 groups in [re_ranged]. *)
let ranged_of_string of_string text =
  let open Result.Syntax in
  match Re.exec_opt re_ranged text with
  | None -> Error (Printf.sprintf {|invalid integer range prefix in "%s"|} text)
  | Some groups ->
     let* iset =
       match Re.Group.get groups 1 with
       | "" -> Ok None
       | iset ->
          let* iset = Sets.Integers.of_string iset in
          Ok (Some iset)
     and* regexp =
       Re.Group.get groups 2 |> of_string in
     Ok (iset, regexp)

