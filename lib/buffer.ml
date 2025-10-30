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
    bpp : int;
    width : int;
    height : int;
    handle : id;
    pitch : int;
    size : int64;
  }

  let create fd ~bpp (width, height) =
    let handle = Ctypes.(allocate_n C.Types.buffer_id ~count:1) in
    let pitch = Ctypes.(allocate_n C.Types.pitch ~count:1) in
    let size = Ctypes.(allocate_n C.Types.size ~count:1) in
    let flags = U32.zero in
    match C.Functions.drmModeCreateDumbBuffer fd width height bpp flags handle pitch size with
    | 0, _ ->
      {
        bpp; width; height;
        handle = !@ handle;
        pitch = !@ pitch;
        size = !@ size;
      }
    | _, errno -> Err.report errno "drmModeCreateDumbBuffer" ""

  let get_map_offset fd handle =
    let offset = Ctypes.(allocate_n uint64_t ~count:1) in
    match C.Functions.drmModeMapDumbBuffer fd handle offset with
    | 0, _ -> U64.to_int64 (!@ offset)
    | _, errno -> Err.report errno "drmModeMapDumbBuffer" ""

  let map fd t kind =
    let expected_bpp = Bigarray.kind_size_in_bytes kind * 8 in
    if expected_bpp <> t.bpp then (
      Fmt.invalid_arg "Requested kind has %d bpp, but buffer is %d bpp" expected_bpp t.bpp
    );
    (* The device might have rounded the width up. Need to map using the actual size.
       This won't work if the pitch isn't a multiple of the kind size, but
       it doesn't seem likely any device would do that. *)
    let width = t.pitch / Bigarray.kind_size_in_bytes kind in
    let offset = get_map_offset fd t.handle in
    map_pseudofile fd ~pos:offset ~kind (width, t.height)

  let destroy fd handle =
    match C.Functions.drmModeDestroyDumbBuffer fd handle with
    | 0, _ -> ()
    | _, errno -> Err.report errno "drmModeDestroyDumbBuffer" ""

  let pp f { bpp; width; height; handle; pitch; size } =
    Fmt.pf f "{ bpp = %d; width = %d; height = %d; handle = %a; pitch = %d; size = %Ld }"
      bpp width height Id.pp handle pitch size
end
