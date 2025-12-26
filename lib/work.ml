module Artist =
  struct
    type t =
      { id : string;
        name : string;
        sort_name : string option;
        part : string option;
        role : string option }
    let of_mb mb =
      let module MB = Musicbrainz.Artist in
      let id = mb.MB.id in
      let name, sort_name =
        match mb.MB.name, mb.MB.sort_name with
        | Some name, sort_name -> (name, sort_name)
        | None, (Some name as sort_name) -> (name, sort_name)
        | None, None -> ("Anonymous", None) in
      let part = None in
      let role = None in
      { id; name; sort_name; part; role }
  end

module Track =
  struct
    type t =
      { id : string;
        number : int;  (** Overall position of the track in the whole work, counting from 1. *)
        title : string;
        performers : Artist.t list }
    let of_mb mb =
      let module MB = Musicbrainz.Artist in
      let id = mb.MB.id in
      let number = 0 in
      let title = "" in
      let performers = [] in
      { id; number; title; performers }
  end

module Partial =
  struct
    type t =
      { release : string; (** The Musicbrainz id of the release from which the data are taken. *)
        disk : string; (** The diskid from which the audio was ripped. *)
        title : string; (** The title of the whole work. This can not be empty. *)
        composers : Artist.t list (** Composers of a work that is expected to be performed
                                      by others.  This will usually be left empty for
                                   popular music.  *);
        performers : Artist.t list; (** Instrumentalists, singers, conductors. *)
        tracks : Track.t list;
        total_tracks : int (** The total number of tracks of the piece.  This is only needed
                               for the correct number of leading zeros in numbers in filenames. *)
      }
  end
(** The part of a a musical work that fits on one disk. *)
