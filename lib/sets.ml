module MBID = Set.Make (String)

let mbid_union sets =
  List.fold_left MBID.union MBID.empty sets

