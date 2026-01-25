
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

let%test _ =
  common_prefix [] = ("", [])

let%test _ =
  common_prefix ["abc"] = ("abc", [""])

let%test _ =
  common_prefix ["abc"; "abd"] = ("ab", ["c"; "d"])
let%test _ =
  common_prefix ["abc"; "abd"] = ("ab", ["c"; "d"])

let%test _ =
  common_prefix ["abc"; "a"; "abd"] = ("a", ["bc"; ""; "bd"])

let%test _ =
  common_prefix ["abc"; ""; "abd"] = ("", ["abc"; ""; "abd"])
