type t = { error : string; help : string option }

let make error help = { error; help }

let jsont =
  Jsont.Object.map ~kind:"Error" make
  |> Jsont.Object.mem "error" Jsont.string
  |> Jsont.Object.opt_mem "help" Jsont.string
  |> Jsont.Object.finish

let get_error_opt text =
  match Jsont_bytesrw.decode_string jsont text with
  | Error _ -> None
  | Ok json -> Some json.error
