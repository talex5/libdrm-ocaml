module U32 = Unsigned.UInt32
module U64 = Unsigned.UInt64

type id = [`Buffer] Id.t

let ( !@ ) = Ctypes.( !@ )

let close dev handle =
  match C.Functions.drmCloseBufferHandle dev handle with
  | 0, _ -> ()
  | _, errno -> Err.report errno "drmCloseBufferHandle" ""

external caml_map_pseudofile :
  Unix.file_descr ->
  ('a, 'b) Bigarray.kind ->
  int64 ->
  int ->
  int ->
  ('a, 'b, Bigarray.c_layout) Bigarray.Array2.t
  = "caml_map_pseudofile"

let map_pseudofile ?(pos=0L) fd ~kind (width, height) =
  caml_map_pseudofile fd kind pos width height

module Dumb = struct
  type t = {
    handle : id;
    pitch : int;
    size : int64;
  }

  let create fd ~bpp ~size:(width, height) =
    let handle = Ctypes.(allocate_n C.Types.buffer_id ~count:1) in
    let pitch = Ctypes.(allocate_n C.Types.pitch ~count:1) in
    let size = Ctypes.(allocate_n C.Types.size ~count:1) in
    let flags = U32.zero in
    match C.Functions.drmModeCreateDumbBuffer fd width height bpp flags handle pitch size with
    | 0, _ ->
      {
        handle = !@ handle;
        pitch = !@ pitch;
        size = !@ size;
      }
    | _, errno -> Err.report errno "drmModeCreateDumbBuffer" ""

  let map fd t kind size =
    let offset = Ctypes.(allocate_n uint64_t ~count:1) in
    match C.Functions.drmModeMapDumbBuffer fd t.handle offset with
    | 0, _ ->
      let offset = U64.to_int64 (!@ offset) in
      map_pseudofile fd ~pos:offset ~kind size
    | _, errno -> Err.report errno "drmModeMapDumbBuffer" ""

  let destroy fd handle =
    match C.Functions.drmModeDestroyDumbBuffer fd handle with
    | 0, _ -> ()
    | _, errno -> Err.report errno "drmModeDestroyDumbBuffer" ""
end
