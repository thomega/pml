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

(** We use a prefix for the WAV file, because discids can start with a period. *)
let wav_prefix = "cd-"

let bitrate = 128

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
  synchronously ?verbose ?dry "opusenc"
    (["--bitrate"; string_of_int bitrate;
      "--tracknumber"; string_of_int tracknumber;
      "--artist"; artist; "--title"; title] @ performers @ [input; output])

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

let encode_track ?dry ?verbose bitrate dir d t =
  let tracknumber = t.Track.number in
  let input = wav_name d tracknumber
  and output =
    Printf.sprintf "%0*d %s.opus" d.track_width tracknumber t.Track.title
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
    Printf.sprintf "%0*d %s: %s"
      d.track_width t.Track.number
      (List.hd d.Tagged.titles |> Tagged.title_to_string) t.Track.title
  and performers =
    Artist.Collection.to_list t.Track.artists
    |> List.map Artist.to_string in
  let output = Filename.concat dir output in
  opusenc ?verbose ?dry ~bitrate ~tracknumber ~artist ~title ~performers ~input ~output ()

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
  (Edit.filename_safe root,
   Filename.concat root (Edit.filename_safe subdir))

let execute ?dry ?verbose ?directory d =
  let open Result.Syntax in
  let* _ = chdir ?directory () in
  let* _ =
    Result_list.fold_left
      (fun _ t -> t.Track.number_on_disc |> rip_track d)
      () d.Tagged.tracks in
  let root, dir = target_dir d in
  let* _ = mkdir root in
  let* _ = mkdir dir in
  let* _ =
    Result_list.fold_left
      (fun _ -> encode_track ?dry ?verbose bitrate dir d)
      () d.tracks in
  Ok ()

(* ********************************************************************** *)
(* Obsolescent: *)
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
