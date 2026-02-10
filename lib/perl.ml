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

module CMap = Map.Make (Char)
module CSet = Set.Make (Char)

let flag_map =
  CMap.of_list [('i', `CASELESS);
                ('x', `EXTENDED)]

let cset_of_string s =
  String.fold_right CSet.add s CSet.empty

let lookup_flag ?(err_prefix="") ?(err_postfix="") map c =
  CMap.find_opt c map
  |> Option.to_result ~none:(err_prefix ^ Printf.sprintf "invalid flag '%c'" c ^ err_postfix)

let flags_of_cset ?err_prefix ?err_postfix map s =
  CSet.elements s |> Result_list.map (lookup_flag ?err_prefix ?err_postfix map)

type 'a ranged' = Sets.Integers.S.t option * 'a

let set_integers =
  Re.(alt [rg '0' '9'; set ",-"])

let re_ranged =
  Re.(seq [start;
           group (rep set_integers);
           group (seq [compl [set_integers]; rep any]);
           stop] |> compile)

let ranged_of_string' of_string text =
  let open Result.Syntax in
  match Re.exec_opt re_ranged text with
  | None -> Error (Printf.sprintf {|ranged_of_string failed: "%s"|} text)
  | Some groups ->
     let* iset =
       match Re.Group.get_opt groups 1 with
       | Some "" -> Ok None
       | Some iset ->
          let* iset = Sets.Integers.of_string iset in
          Ok (Some iset)
       | None -> Error ("")
     and* regexp =
       match Re.Group.get_opt groups 2 with
       | Some regexp -> of_string regexp
       | None -> Error ("") in
     Ok (iset, regexp)

module M =
  struct

    type t =
      { rex : Pcre2.regexp;
        text: string }

    let to_string p =
      p.text

    let myname = "Perl.M"
    let err_prefix = myname ^ ": "

    let split_substitution s =
      let l = String.length s in
      if l = 0 then
        Error (myname ^ ": empty string")
      else
        let delim = s.[0] in
        match String.split_on_char delim (String.sub s 1 (pred l)) with
        | [] | [_] -> Error (Printf.sprintf {|%s: missing second '%c' in "%s"|} myname delim s)
        | [rex; ""] -> Ok (rex, "")
        | [rex; flags] -> Ok (rex, flags)
        | _ -> Error (Printf.sprintf {|%s: too many '%c's in "%s"|} myname delim s)

    let%test _ = split_substitution "/ab" = Error (err_prefix ^ {|missing second '/' in "/ab"|})
    let%test _ = split_substitution "/ab/" = Ok ("ab", "")
    let%test _ = split_substitution "/a/b" = Ok ("a", "b")
    let%test _ = split_substitution "/a/b/" = Error (err_prefix ^ {|too many '/'s in "/a/b/"|})

    let of_string text =
      let open Result.Syntax in
      let err_postfix = {| in "|} ^ text ^ {|"|} in
      let* rex, flags = split_substitution text in
      let flags = cset_of_string flags in
      let* flags = flags_of_cset ~err_prefix ~err_postfix flag_map flags in
      let* rex =
        try
          Ok (Pcre2.regexp ~flags:(`UTF :: flags) rex)
        with
        | e -> Error (Printf.sprintf {|"%s": %s|} text (Printexc.to_string e)) in
      Ok { rex; text }

    let exec { rex; _ } s =
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
    let%test _ = exec' "| a b|" "aab" = Ok false
    let%test _ = exec' "| a b|x" "aab" = Ok true
    let%test _ = exec' "| a b|xx" "aab" = Ok true
    let%test _ = exec' "| a b|xix" "aab" = Ok true
    let%test _ = exec' "/a/Ix" "Aa" = Error (err_prefix ^ {|invalid flag 'I' in "/a/Ix"|})

    type ranged = t ranged'

    let ranged_of_string text =
      ranged_of_string' of_string text

    let%test_module _ =
      (module struct

         let expect s (ilist_opt, regexp) =
           let open Result.Syntax in
           match
             let* iset, re = ranged_of_string s in
             match iset, ilist_opt with
             | None, None -> Ok (to_string re = regexp)
             | None, Some _ | Some _, None -> Ok false
             | Some iset, Some ilist ->
                Ok (Sets.Integers.S.elements iset = List.sort Int.compare ilist &&
                  to_string re = regexp)
           with
           | Error msg -> prerr_endline msg; false
           | Ok tof -> tof

         let%test _ = expect "/a2/" (None, "/a2/")
         let%test _ = expect "2-1/a1/" (Some [], "/a1/")
         let%test _ = expect "1/a/i" (Some [1], "/a/i")
         let%test _ = expect "1,3/a/x" (Some [1;3], "/a/x")
         let%test _ = expect "1-2/a/" (Some [1;2], "/a/")

       end)

    let ranged_to_string (ranges, perl_m) =
      match ranges with
      | Some ranges -> Sets.Integers.to_string ranges ^ to_string perl_m
      | None -> to_string perl_m

  end

module S =
  struct

    type t =
      { rex : Pcre2.regexp;
        sub : Pcre2.substitution;
        global : bool;
        text: string }

    let to_string p =
      p.text

    let myname = "Perl.S"
    let err_prefix = myname ^ ": "

    let split_substitution s =
      let l = String.length s in
      if l = 0 then
        Error (myname ^ ": empty string")
      else
        let delim = s.[0] in
        match String.split_on_char delim (String.sub s 1 (pred l)) with
        | [] | [_] -> Error (Printf.sprintf {|%s: missing second '%c' in "%s"|} myname delim s)
        | [_; _] -> Error (Printf.sprintf {|%s: missing third '%c' in "%s"|} myname delim s)
        | [rex; sub; ""] -> Ok (rex, sub, "")
        | [rex; sub; flags] -> Ok (rex, sub, flags)
        | _ -> Error (Printf.sprintf {|%s: too many '%c's in "%s"|} myname delim s)

    let%test _ = split_substitution "/ab" = Error (err_prefix ^ {|missing second '/' in "/ab"|})
    let%test _ = split_substitution "/a/b" = Error (err_prefix ^ {|missing third '/' in "/a/b"|})
    let%test _ = split_substitution "/a/b/" = Ok ("a", "b", "")
    let%test _ = split_substitution "/a//" = Ok ("a", "", "")
    let%test _ = split_substitution "/a/b/c" = Ok ("a", "b", "c")
    let%test _ = split_substitution "/a//c" = Ok ("a", "", "c")
    let%test _ = split_substitution "/a/b/c/" = Error (err_prefix ^ {|too many '/'s in "/a/b/c/"|})

    let of_string text =
      let open Result.Syntax in
          let err_postfix = {| in "|} ^ text ^ {|"|} in
      let* rex, sub, flags = split_substitution text in
      let flags = cset_of_string flags in
      let global = CSet.mem 'g' flags
      and flags = CSet.remove 'g' flags in
      let* flags = flags_of_cset ~err_prefix ~err_postfix flag_map flags in
      let* rex =
        try
          Ok (Pcre2.regexp ~flags:(`UTF :: flags) rex)
        with
        | e -> Error (Printf.sprintf {|"%s": %s|} text (Printexc.to_string e))
      and* sub =
        try
          Ok (Pcre2.subst sub)
        with
        | e -> Error (Printf.sprintf {|"%s": %s|} text (Printexc.to_string e)) in
      Ok { rex; sub; global; text }

    let exec { rex; sub; global; _ } s =
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
    let%test _ = exec' "/ a /b/" "aa" = Ok "aa"
    let%test _ = exec' "/ a /b/x" "aa" = Ok "ba"
    let%test _ = exec' "/ a /b/xg" "aa" = Ok "bb"
    let%test _ = exec' "/ a /b/xgi" "AA" = Ok "bb"
    let%test _ = exec' "/a/b/i" "Aa" = Ok "ba"
    let%test _ = exec' "/a/b/g" "aa" = Ok "bb"
    let%test _ = exec' "|a|b|g" "aa" = Ok "bb"
    let%test _ = exec' "/a/b/ig" "Aa" = Ok "bb"
    let%test _ = exec' "/a/b/I" "Aa" = Error (err_prefix ^ {|invalid flag 'I' in "/a/b/I"|})
    let%test _ = exec' "/[Дт]/X/g" "Дмитрий" = Ok "XмиXрий"

    type ranged = t ranged'

    let ranged_of_string text =
      ranged_of_string' of_string text

    let%test_module _ =
      (module struct

         let expect s (ilist_opt, regexp) =
           let open Result.Syntax in
           match
             let* iset, re = ranged_of_string s in
             match iset, ilist_opt with
             | None, None -> Ok (to_string re = regexp)
             | None, Some _ | Some _, None -> Ok false
             | Some iset, Some ilist ->
                Ok (Sets.Integers.S.elements iset = List.sort Int.compare ilist &&
                  to_string re = regexp)
           with
           | Error msg -> prerr_endline msg; false
           | Ok tof -> tof

         let%test _ = expect "/a2/b/" (None, "/a2/b/")
         let%test _ = expect "2-1/a1/b/" (Some [], "/a1/b/")
         let%test _ = expect "1/a/b/i" (Some [1], "/a/b/i")
         let%test _ = expect "1,3/a/b/x" (Some [1;3], "/a/b/x")
         let%test _ = expect "1-2/a/b/" (Some [1;2], "/a/b/")

       end)

    let ranged_to_string (ranges, perl_s) =
      match ranges with
      | Some ranges -> Sets.Integers.to_string ranges ^ to_string perl_s
      | None -> to_string perl_s

  end


