type t =
  { id : string;
    freedb : string;
    toc : string }

val get : ?device:string -> unit -> (t, string) Result.t
