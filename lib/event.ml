type buffer = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

(* Note: libdrm's drmEventContext uses both "unsigned int" and "uint64_t" for sequence numbers.
   The kernel uses __u32 and __u64. *)

type vblank_handler =
  sequence:Unsigned.UInt32.t ->
  tv_sec:Unsigned.UInt32.t ->
  tv_usec:int ->
  crtc_id:Kms.Crtc.id ->
  user_data:nativeint ->
  unit

type sequence_handler =
  sequence:Unsigned.UInt64.t ->
  time_ns:Unsigned.UInt64.t ->          (* Kernel API says signed, libdrm says unsigned *)
  user_data:nativeint ->
  unit

let ( !@ ) = Ctypes.( !@ )

let create_buffer () = Bigarray.Array1.create Char C_layout 1024

let handle_vblank data (cb : vblank_handler) =
  let module T = C.Types.Drm_event_vblank in
  assert (Ctypes.CArray.length data >= Ctypes.sizeof T.t);
  let t = !@ (Ctypes.from_voidp T.t (Ctypes.to_voidp data.astart)) in
  cb
    ~sequence:(Ctypes.getf t T.sequence)
    ~tv_sec:(Ctypes.getf t T.tv_sec)
    ~tv_usec:(Ctypes.getf t T.tv_usec)
    ~crtc_id:(Ctypes.getf t T.crtc_id)
    ~user_data:(Ctypes.getf t T.user_data)

let handle_sequence data (cb : sequence_handler) =
  let module T = C.Types.Drm_event_crtc_sequence in
  assert (Ctypes.CArray.length data >= Ctypes.sizeof T.t);
  let t = !@ (Ctypes.from_voidp T.t (Ctypes.to_voidp data.astart)) in
  cb
    ~sequence:(Ctypes.getf t T.sequence)
    ~time_ns:(Unsigned.UInt64.of_int64 (Ctypes.getf t T.time_ns))
    ~user_data:(Ctypes.getf t T.user_data)

let parse ?vblank ?flip_complete ?sequence_handler ?unknown_event buffer len =
  let rec aux buffer =
    if Ctypes.CArray.length buffer > 0 then (
      assert (Ctypes.CArray.length buffer >= Ctypes.sizeof C.Types.Drm_event.t);
      let hdr = !@ (Ctypes.from_voidp C.Types.Drm_event.t (Ctypes.to_voidp buffer.astart)) in
      let event_type = Ctypes.getf hdr C.Types.Drm_event.typ in
      let event_len = Ctypes.getf hdr C.Types.Drm_event.length in
      let event_data = Ctypes.CArray.sub buffer ~pos:0 ~length:event_len in
      let module T = C.Types.Drm_event_type in
      if event_type = T.vblank then Option.iter (handle_vblank event_data) vblank
      else if event_type = T.flip_complete then Option.iter (handle_vblank event_data) flip_complete
      else if event_type = T.crtc_sequence then Option.iter (handle_sequence event_data) sequence_handler
      else Option.iter (fun cb -> cb event_type) unknown_event;
      let buffer = Ctypes.CArray.sub buffer ~pos:event_len ~length:(Ctypes.CArray.length buffer - event_len) in
      aux buffer
    )
  in
  buffer
  |> Ctypes.(array_of_bigarray array1)
  |> Ctypes.CArray.sub ~pos:0 ~length:len
  |> aux;
  ignore (Sys.opaque_identity buffer)   (* Ensure buffer isn't GC'd until now *)
