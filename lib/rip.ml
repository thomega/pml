(* rip.ml -- part of PML (Physical Media Library)

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

type encoder =
  | Opus
  | Vorbis
  | Flac
  | Mp3

let encoder_to_string = function
  | Opus -> "opus"
  | Vorbis -> "vorbis"
  | Flac -> "flac"
  | Mp3 -> "mp3"

let encoders =
  [Opus; Vorbis; Flac; Mp3]

(** We use a prefix for the WAV file, because discids can start with a period. *)
let wav_prefix = "cd-"

let re_white =
  Re.(set " \t\n\r" |> compile)

let quote_if_white s =
  if Re.execp re_white s then
    {|"|}^ s ^ {|"|}
  else
    s

let synchronously ?(verbose=false) ?(dry=false) program args =
  let open Unix in
  let open Printf in
  let verbose = verbose || dry in
  if verbose then
    begin
      let args = List.map quote_if_white args |> String.concat " " in
      printf "executing: %s %s\n" program args;
      flush Stdlib.stdout
    end;
  if dry then
    Ok ()
  else
    let pid = create_process program (program :: args |> Array.of_list) stdin stdout stderr in
    match waitpid [] pid with
    | _, WEXITED 0 -> Ok ()
    | _, WEXITED rc -> Error (sprintf "%s returned %d" program rc)
    | _, WSTOPPED s -> Error (sprintf "%s stopped by signal %s" program (Sys.signal_to_string s))
    | _, WSIGNALED s -> Error (sprintf "%s killed by signal %s" program (Sys.signal_to_string s))

let opusenc ?verbose ?dry ~bitrate ~tracknumber ~artist ~title ~performers ~input ~output () =
  let performers =
    List.concat_map (fun performer -> ["--comment"; "performer=" ^ performer]) performers in
  let output = output ^ ".opus" in
  synchronously ?verbose ?dry "opusenc"
    (["--quiet";
      "--bitrate"; string_of_int bitrate;
      "--tracknumber"; string_of_int tracknumber;
      "--artist"; artist; "--title"; title] @ performers @ [input; output])

let vorbisenc ?verbose ?dry ~bitrate ~tracknumber ~artist ~title ~performers ~input ~output () =
  ignore tracknumber;
  let performers =
    List.concat_map (fun performer -> ["--comment"; "performer=" ^ performer]) performers in
  let output = output ^ ".ogg" in
  synchronously ?verbose ?dry "oggenc"
    (["--quiet";
      "--bitrate"; string_of_int bitrate;
      "--artist"; artist; "--title"; title] @ performers @ ["--output"; output; input])

let flacenc ?verbose ?dry ~tracknumber ~artist ~title ~performers ~input ~output () =
  let performers =
    List.concat_map (fun performer -> ["--tag"; "PERFORMER=" ^ performer]) performers in
  let output = output ^ ".oga" in
  synchronously ?verbose ?dry "flac"
    (["--silent";
      "--force";
      "--ogg";
      "--tag"; "TRACKNUMBER=" ^ string_of_int tracknumber;
      "--tag"; "ARTIST=" ^ artist;
      "--tag"; "TITLE=" ^title] @
       performers @ ["--output-name"; output; input])

(** Derive lame quality option from [bitrate] *)
(** German Wikipedia: https://de.wikipedia.org/wiki/LAME *)
let lame_quality_bitrate =
  [(0, 245);
   (1, 225);
   (2, 190);
   (3, 175);
   (4, 165);
   (5, 130);
   (6, 115);
   (7, 100);
   (8,  85)]

let lame_quality_of_bitrate bitrate =
  let rec lame_quality_of_bitrate' = function
    | [] -> 9.999
    | [(_, _b)] -> 9.999
    | (qhi, bhi) :: ((qlo, blo) :: _ as remaining) ->
       if bitrate > bhi then
         float_of_int qhi
       else if bitrate >= blo then
         let frac = float_of_int (bitrate - blo) /. float_of_int (bhi - blo) in
         float_of_int qlo +. frac *. float_of_int (qhi - qlo)
       else
         lame_quality_of_bitrate' remaining in
  lame_quality_of_bitrate' lame_quality_bitrate

let%test _ = lame_quality_of_bitrate 320 = 0.
let%test _ = lame_quality_of_bitrate 245 = 0.
let%test _ = lame_quality_of_bitrate 225 = 1.
let%test _ = lame_quality_of_bitrate 190 = 2.
let%test _ = lame_quality_of_bitrate 175 = 3.
let%test _ = lame_quality_of_bitrate 165 = 4.
let%test _ = lame_quality_of_bitrate 130 = 5.
let%test _ = lame_quality_of_bitrate 115 = 6.
let%test _ = lame_quality_of_bitrate 100 = 7.
let%test _ = lame_quality_of_bitrate 85 = 8.

let mp3enc ?verbose ?dry ~bitrate ~tracknumber ~artist ~title ~performers ~input ~output () =
  let quality = lame_quality_of_bitrate bitrate in
  let output = output ^ ".mp3" in
  synchronously ?verbose ?dry "lame"
    (["--quiet";
      (* "--id3v2-only"; *)
      "-V"; string_of_float quality;
      "--tn"; string_of_int tracknumber;
      "--ta"; artist;
      "--tt"; title;
      "--tc"; String.concat "; " performers] @ [input; output])

let wav_name d i =
  Printf.sprintf "%s%s%02d.wav" wav_prefix d.Tagged.discid i

let rip_track ?(force=false) d i =
  let wav = wav_name d i in
  let args = ["-w"; string_of_int i; wav] in
  if Sys.file_exists wav then
    if Sys.is_regular_file wav then
      if force then
        synchronously "cdparanoia" args
      else
        begin
          Printf.printf "not ripping %s again\n" wav;
          flush stdout;
          Ok ()
        end
    else
      Error (Printf.sprintf "%s exists and is not a regular file" wav)
  else
    synchronously "cdparanoia" args

let encode_track ?dry ?verbose bitrate encoders dir d t =
  let tracknumber = t.Track.number in
  let input = wav_name d tracknumber
  and output =
    Printf.sprintf "%0*d %s" d.track_width tracknumber t.Track.title
    |> Edit.filename_safe in
  let artist =
    match d.Tagged.composer with
    | Some a -> a.Artist.name
    | None ->
       begin match Artist.Collection.min_elt_opt t.Track.artists with
       | Some p -> p.Artist.name
       | None -> "Anonymous"
       end
  and title =
    Printf.sprintf "%s: %s" (List.hd d.Tagged.titles |> Tagged.title_to_string) t.Track.title
  and performers =
    Artist.Collection.to_list t.Track.artists |> List.map Artist.to_string in
  let output = Filename.concat dir output in
  Result_list.iter
    (function
     | Opus ->
        opusenc ?verbose ?dry ~bitrate ~tracknumber ~artist ~title ~performers ~input ~output ()
     | Vorbis ->
        vorbisenc ?verbose ?dry ~bitrate ~tracknumber ~artist ~title ~performers ~input ~output ()
     | Flac ->
        flacenc ?verbose ?dry ~tracknumber ~artist ~title ~performers ~input ~output ()
     | Mp3 ->
        mp3enc ?verbose ?dry ~bitrate ~tracknumber ~artist ~title ~performers ~input ~output ())
    encoders

let mkdir name =
  if Sys.file_exists name then
    if Sys.is_directory name then
      Ok ()
    else
      Error ("not a directory: " ^ name)
  else
    try
      Ok (Sys.mkdir name 0o755)
    with
    | e -> Error (Printexc.to_string e)

let chdir ?directory () =
  match directory with
  | None -> Ok ()
  | Some name ->
     try
       Unix.chdir name;
       Ok ()
     with
     | e -> Error (Printexc.to_string e)

let target_dir d =
  let root =
    match d.Tagged.composer with
    | Some c -> c.Artist.sort_name
    | None -> "Anonymous"
  and subdir =
    match d.Tagged.titles, d.Tagged.performer with
    | [], None -> "Unnamed"
    | t :: _, None -> (Tagged.title_to_string t)
    | [], Some p -> p.Artist.sort_name
    | t :: _, Some p -> Tagged.title_to_string t ^ " - " ^ p.Artist.sort_name in
  let root = Edit.filename_safe root
  and subdir = Edit.filename_safe subdir in
  (root, Filename.concat root subdir)

let execute ?dry ?verbose ?directory ~bitrate encoders d =
  let open Result.Syntax in
  let* () = chdir ?directory () in
  let* () = Result_list.iter (fun t -> t.Track.number_on_disc |> rip_track d) d.Tagged.tracks in
  let root, dir = target_dir d in
  let* () = mkdir root in
  let* () = mkdir dir in
  Result_list.iter (encode_track ?dry ?verbose bitrate encoders dir d) d.tracks

(* ********************************************************************** *)
(* Obsolescent: *)
let bitrate = 128

let shell_quote =
  Edit.shell_double_quote

let quoted_filename s =
  Edit.filename_safe s |> shell_quote

let encode_track d t =
  let open Printf in
  printf "opusenc --bitrate %d \\\n" bitrate;
  printf "  --tracknumber %d \\\n" t.Track.number;
  begin match d.Tagged.composer with
  | Some a -> printf "  --artist %s \\\n" (shell_quote a.Artist.name)
  | None -> ()
  end;
  let track =
    sprintf "%0*d %s: %s"
      d.track_width t.Track.number
      (List.hd d.Tagged.titles |> Tagged.title_to_string) t.Track.title in
  printf "  --title %s \\\n" (shell_quote track);
  Artist.Collection.iter
    (fun p -> printf "  --comment performer=%s \\\n" (Artist.to_string p |> shell_quote))
    t.Track.artists;
  printf "  \"$WAV\" \"$DIR/$TRACK.opus\"\n"

let script d =
  let open Printf in
  let wav_name i = sprintf "%s%s%02d.wav" wav_prefix d.Tagged.discid i in
  let separator () =
    printf "########################################################################\n" in
  printf "#! /bin/sh\n";
  separator ();
  printf "DISCID=%s\n" d.Tagged.discid;
  printf "MEDIUM=%s\n" d.Tagged.medium_id;
  printf "RELEASE=%s\n" d.Tagged.release_id;
  separator ();
  printf "\n";
  separator ();
  printf "# Rip CD track unless the output file exists\n";
  separator ();
  printf "\n";
  printf "rip_track () {\n";
  printf "  if [ ! -r $2 ]; then\n";
  printf "    cdparanoia -w $1 $2\n";
  printf "  fi\n";
  printf "}\n";
  printf "\n";
  List.iter
    (fun t ->
      let n = t.Track.number_on_disc in
      printf "rip_track %2d %s\n" n (wav_name n))
    d.Tagged.tracks;
  printf "\n";
  separator ();
  printf "# Set up target directory\n";
  separator ();
  printf "\n";
  let root =
    match d.composer with
    | Some c -> c.Artist.sort_name
    | None -> "Anonymous" in
  printf "ROOT=%s\n" (shell_quote root);
  let subdir =
    match d.Tagged.titles, d.Tagged.performer with
    | [], None -> "Unnamed"
    | t :: _, None -> (Tagged.title_to_string t)
    | [], Some p -> p.Artist.sort_name
    | t :: _, Some p -> sprintf "%s - %s" (Tagged.title_to_string t) p.Artist.sort_name in
  printf "SUBDIR=%s\n" (quoted_filename subdir);
  printf "DIR=\"$ROOT/$SUBDIR\"\n";
  printf "mkdir -p \"$DIR\"\n";
  printf "\n";
  separator ();
  printf "# Encode and tag\n";
  separator ();
  printf "\n";
  List.iter
    (fun t ->
      printf "WAV=%s\n" (wav_name t.Track.number_on_disc);
      let track = sprintf "%0*d %s" d.track_width t.Track.number t.Track.title in
      printf "TRACK=%s\n" (quoted_filename track);
      encode_track d t;
      printf "\n")
    d.tracks;
  Ok ()
