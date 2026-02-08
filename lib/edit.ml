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
let re_rep_white =
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
let filename_safe s =
  s
  |> Re.replace_string re_slash ~by:"-"
  |> Re.replace_string re_colon ~by:" -"
  |> Re.replace_string re_double_quote ~by:"'"
  |> Re.replace_string re_rep_white ~by:" "
  |> Re.replace_string re_boundary_white ~by:""

let%test _ = filename_safe {|a: b|} = {|a - b|}
let%test _ = filename_safe {|a  /  b|} = {|a - b|}
let%test _ = filename_safe {| a  \  b|} = {|a - b|}
let%test _ =
filename_safe {|a
\/  b
c |} = {|a -- b c|}

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

type perl_s =
  { rex : Pcre2.regexp;
    sub : Pcre2.substitution;
    global : bool }

let split_substitution s =
  let myname = "perl_s_of_string" in
  let l = String.length s in
  if l = 0 then
    Error (myname ^ ": empty string")
  else
    let delim = s.[0] in
    match String.split_on_char delim (String.sub s 1 (pred l)) with
    | [] | [_] -> Error (Printf.sprintf "%s: missing second '%c'" myname delim)
    | [_; _] -> Error (Printf.sprintf "%s: missing third '%c'" myname delim)
    | [rex; sub; ""] -> Ok (rex, sub, "")
    | [rex; sub; flags] -> Ok (rex, sub, flags)
    | _ -> Error (Printf.sprintf "%s: too many '%c's" myname delim)

let%test _ = split_substitution "/ab" = Error ("perl_s_of_string: missing second '/'")
let%test _ = split_substitution "/a/b" = Error ("perl_s_of_string: missing third '/'")
let%test _ = split_substitution "/a/b/" = Ok ("a", "b", "")
let%test _ = split_substitution "/a//" = Ok ("a", "", "")
let%test _ = split_substitution "/a/b/c" = Ok ("a", "b", "c")
let%test _ = split_substitution "/a//c" = Ok ("a", "", "c")
let%test _ = split_substitution "/a/b/c/" = Error ("perl_s_of_string: too many '/'s")

let perl_s_of_string s =
  let open Result.Syntax in
  let* rex, sub, flags = split_substitution s in
  let global = String.contains flags 'g'
  and caseless = String.contains flags 'i' in
  let* rex =
    try
      let flags =
        if caseless then
          [`CASELESS]
        else
          [] in
      Ok (Pcre2.regexp ~flags rex)
    with
    | e -> Error (Printexc.to_string e)
  and* sub =
    try
      Ok (Pcre2.subst sub)
    with
    | e -> Error (Printexc.to_string e) in
  Ok { rex; sub; global }

let perl_s { rex; sub; global } s =
  try
    if global then
      Ok (Pcre2.replace ~rex ~itempl:sub s)
    else
      Ok (Pcre2.replace_first ~rex ~itempl:sub s)
  with
  | e -> Error (Printexc.to_string e)

let perl_s' expr s =
  let open Result.Syntax in
  let* expr = perl_s_of_string expr in
  perl_s expr s

let%test _ = perl_s' "/a/b/" "aa" = Ok ("ba")
let%test _ = perl_s' "/a/b/g" "aa" = Ok ("bb")
let%test _ = perl_s' "|a|b|g" "aa" = Ok ("bb")
