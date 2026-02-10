(* perl.ml -- part of PML (Physical Media Library)

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

module CSet = Set.Make (Char)

let cset_of_string s =
  String.fold_right CSet.add s CSet.empty

let all_chars_in cset s =
  String.to_seq s |> Seq.exists (fun c -> not (CSet.mem c cset))

module M =
  struct

    let allowed_flags = cset_of_string "i"

    type t =
      { rex : Pcre2.regexp;
        text: string }

    let to_string p =
      p.text

    let myname = "Perl.M"

    let split_substitution s =
      let l = String.length s in
      if l = 0 then
        Error (myname ^ ": empty string")
      else
        let delim = s.[0] in
        match String.split_on_char delim (String.sub s 1 (pred l)) with
        | [] | [_] -> Error (Printf.sprintf "%s: missing second '%c' in \"%s\"" myname delim s)
        | [rex; ""] -> Ok (rex, "")
        | [rex; flags] -> Ok (rex, flags)
        | _ -> Error (Printf.sprintf "%s: too many '%c's in \"%s\"" myname delim s)

    let%test _ = split_substitution "/ab" = Error ("Perl.M: missing second '/' in \"/ab\"")
    let%test _ = split_substitution "/ab/" = Ok ("ab", "")
    let%test _ = split_substitution "/a/b" = Ok ("a", "b")
    let%test _ = split_substitution "/a/b/" = Error ("Perl.M: too many '/'s in \"/a/b/\"")

    let of_string text =
      let open Result.Syntax in
      let* rex, flags = split_substitution text in
      if all_chars_in allowed_flags flags then
        Error (Printf.sprintf "%s: invalid flags \"%s\" in \"%s\"" myname flags text)
      else
        let caseless = String.contains flags 'i' in
        let* rex =
          try
            let flags =
              if caseless then
                [`CASELESS; `UTF]
              else
                [`UTF] in
            Ok (Pcre2.regexp ~flags rex)
          with
          | e -> Error (Printf.sprintf "\"%s\": %s" text (Printexc.to_string e)) in
        Ok { rex; text }

    let exec { rex; text } s =
      ignore text;
      try
        Pcre2.pmatch ~rex s
      with
      | _ -> false

    let exec' text s =
      let open Result.Syntax in
      let* expr = of_string text in
      Ok (exec expr s)

    let%test _ = exec' "/a/" "aa" = Ok true
    let%test _ = exec' "/a/" "AA" = Ok false
    let%test _ = exec' "/a/" "Aa" = Ok true
    let%test _ = exec' "/a/i" "AA" = Ok true
    let%test _ = exec' "|ab|" "aa" = Ok false
    let%test _ = exec' "|ab|" "aab" = Ok true
    let%test _ = exec' "/a/x" "Aa" = Error "Perl.M: invalid flags \"x\" in \"/a/x\""

  end

module S =
  struct

    let allowed_flags = cset_of_string "ig"

    type t =
      { rex : Pcre2.regexp;
        sub : Pcre2.substitution;
        global : bool;
        text: string }

    let to_string p =
      p.text

    let myname = "Perl.S"

    let split_substitution s =
      let l = String.length s in
      if l = 0 then
        Error (myname ^ ": empty string")
      else
        let delim = s.[0] in
        match String.split_on_char delim (String.sub s 1 (pred l)) with
        | [] | [_] -> Error (Printf.sprintf "%s: missing second '%c' in \"%s\"" myname delim s)
        | [_; _] -> Error (Printf.sprintf "%s: missing third '%c' in \"%s\"" myname delim s)
        | [rex; sub; ""] -> Ok (rex, sub, "")
        | [rex; sub; flags] -> Ok (rex, sub, flags)
        | _ -> Error (Printf.sprintf "%s: too many '%c's in \"%s\"" myname delim s)

    let%test _ = split_substitution "/ab" = Error ("Perl.S: missing second '/' in \"/ab\"")
    let%test _ = split_substitution "/a/b" = Error ("Perl.S: missing third '/' in \"/a/b\"")
    let%test _ = split_substitution "/a/b/" = Ok ("a", "b", "")
    let%test _ = split_substitution "/a//" = Ok ("a", "", "")
    let%test _ = split_substitution "/a/b/c" = Ok ("a", "b", "c")
    let%test _ = split_substitution "/a//c" = Ok ("a", "", "c")
    let%test _ = split_substitution "/a/b/c/" = Error ("Perl.S: too many '/'s in \"/a/b/c/\"")

    let of_string text =
      let open Result.Syntax in
      let* rex, sub, flags = split_substitution text in
      if all_chars_in allowed_flags flags then
        Error (Printf.sprintf "%s: invalid flags \"%s\" in \"%s\"" myname flags text)
      else
        let global = String.contains flags 'g'
        and caseless = String.contains flags 'i' in
        let* rex =
          try
            let flags =
              if caseless then
                [`CASELESS; `UTF]
              else
                [`UTF] in
            Ok (Pcre2.regexp ~flags rex)
          with
          | e -> Error (Printf.sprintf "\"%s\": %s" text (Printexc.to_string e))
        and* sub =
          try
            Ok (Pcre2.subst sub)
          with
          | e -> Error (Printf.sprintf "\"%s\": %s" text (Printexc.to_string e)) in
        Ok { rex; sub; global; text }

    let exec { rex; sub; global; text } s =
      ignore text;
      try
        if global then
          Ok (Pcre2.replace ~rex ~itempl:sub s)
        else
          Ok (Pcre2.replace_first ~rex ~itempl:sub s)
      with
      | e -> Error (Printexc.to_string e)

    let exec' text s =
      let open Result.Syntax in
      let* expr = of_string text in
      exec expr s

    let%test _ = exec' "/a/b/" "aa" = Ok "ba"
    let%test _ = exec' "/a/b/" "Aa" = Ok "Ab"
    let%test _ = exec' "/a/b/i" "Aa" = Ok "ba"
    let%test _ = exec' "/a/b/g" "aa" = Ok "bb"
    let%test _ = exec' "|a|b|g" "aa" = Ok "bb"
    let%test _ = exec' "/a/b/ig" "Aa" = Ok "bb"
    let%test _ = exec' "/a/b/x" "Aa" = Error "Perl.S: invalid flags \"x\" in \"/a/b/x\""
    let%test _ = exec' "/[Дт]/X/g" "Дмитрий" = Ok "XмиXрий"

  end


