module type Raw =
  sig
    type disc
    val alloc : unit -> disc
    val free : disc -> unit
    val read_sparse : disc -> string option -> int -> int
    val get_error_msg : disc -> string
    val get_id : disc -> string
    val get_freedb_id : disc -> string
  end

module Raw : Raw =
  struct
    type disc = unit Ctypes_static.ptr
    let alloc = Libdiscid.Functions.alloc
    let free = Libdiscid.Functions.free
    let read_sparse = Libdiscid.Functions.read_sparse
    let get_error_msg = Libdiscid.Functions.get_error_msg
    let get_id = Libdiscid.Functions.get_id
    let get_freedb_id = Libdiscid.Functions.get_freedb_id
  end

let get ?device () =
  let open Raw in
  let disc = alloc () in
  match read_sparse disc device 0 with
  | 0 ->
     free disc;
     Result.Error ("libdiscid::read_sparse failed: " ^ get_error_msg disc)
  | _ ->
     let id = get_id disc in
     free disc;
     Result.Ok id

let get_freedb ?device () =
  let open Raw in
  let disc = alloc () in
  match read_sparse disc device 0 with
  | 0 ->
     free disc;
     Result.Error ("libdiscid::read_sparse failed: " ^ get_error_msg disc)
  | _ ->
     let id = get_freedb_id disc in
     free disc;
     Result.Ok id

