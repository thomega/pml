type t =
  { id : string;
    freedb : string;
    toc : string }

val get : ?device:string -> unit -> (t, string) Result.t
val default_device : unit -> string
