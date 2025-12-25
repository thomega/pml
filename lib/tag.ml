module Artist =
  struct
    type t =
      { id : string;
        name : string;
        sort_name : string }
  end

module Composition =
  struct
    type t =
      { id : string;
        title : string;
        composers : Artist.t list }
  end

module Track =
  struct
    type t =
      { id : string;
        title : string;
        performers : Artist.t list }
  end

module Disc =
  struct
    type t =
      { id : string;
        tracks : Track.t list }
  end
