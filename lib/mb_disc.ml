type t =
  { id : string (** While this is optional in the DTD, it should be there anyway. *);
  }

let make id =
  { id }

let jsont =
  Jsont.Object.map ~kind:"Disc" make
  |> Jsont.Object.mem "id" Jsont.string
  |> Jsont.Object.finish


