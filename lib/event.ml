type buffer = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

(* Note: libdrm's drmEventContext uses both "unsigned int" and "uint64_t" for sequence numbers.
   The kernel uses __u32 and __u64. *)

let ( !@ ) = Ctypes.( !@ )

module Vblank = struct
  type t = {
    sequence : Unsigned.UInt32.t;
    tv_sec : Unsigned.UInt32.t;
    tv_usec : int;
    crtc_id : Kms.Crtc.id;
    user_data : nativeint;
  }

  let of_c c =
    let module T = C.Types.Drm_event_vblank in
    assert (Ctypes.CArray.length c >= Ctypes.sizeof T.t);
    let t = !@ (Ctypes.from_voidp T.t (Ctypes.to_voidp c.astart)) in
    {
      sequence = Ctypes.getf t T.sequence;
      tv_sec = Ctypes.getf t T.tv_sec;
      tv_usec = Ctypes.getf t T.tv_usec;
      crtc_id = Ctypes.getf t T.crtc_id;
      user_data = Ctypes.getf t T.user_data;
    }

  let pp f { sequence; tv_sec; tv_usec; crtc_id; user_data } =
    Fmt.pf f "{@[sequence = %a;@ tv_sec,tv_usec = %a.%06d;@ crtc_id = %a;@ user_data = %nd@]}"
        Unsigned.UInt32.pp sequence
        Unsigned.UInt32.pp tv_sec tv_usec
        Id.pp crtc_id
        user_data
end

module Crtc_sequence = struct
  type t = {
    sequence : Unsigned.UInt64.t;
    time_ns : Unsigned.UInt64.t;          (* Kernel API says signed, libdrm says unsigned *)
    user_data : nativeint;
  }

  let of_c c =
    let module T = C.Types.Drm_event_crtc_sequence in
    assert (Ctypes.CArray.length c >= Ctypes.sizeof T.t);
    let t = !@ (Ctypes.from_voidp T.t (Ctypes.to_voidp c.astart)) in
    {
      sequence = Ctypes.getf t T.sequence;
      time_ns = Unsigned.UInt64.of_int64 (Ctypes.getf t T.time_ns);
      user_data = Ctypes.getf t T.user_data;
    }

  let pp f { sequence; time_ns; user_data } =
    Fmt.pf f "{@[sequence = %a;@ time_ns = %a;@ user_data = %nd@]}"
        Unsigned.UInt64.pp sequence
        Unsigned.UInt64.pp time_ns
        user_data
end

type t =
  | Vblank of Vblank.t
  | Flip_complete of Vblank.t
  | Crtc_sequence of Crtc_sequence.t
  | Unknown of Unsigned.UInt32.t

let pp f = function
  | Vblank e -> Fmt.pf f "Vblank %a" Vblank.pp e
  | Flip_complete e -> Fmt.pf f "Flip_complete %a" Vblank.pp e
  | Crtc_sequence e -> Fmt.pf f "Crtc_sequence %a" Crtc_sequence.pp e
  | Unknown x -> Fmt.pf f "Unknown %a" Unsigned.UInt32.pp x

let create_buffer () = Bigarray.Array1.create Char C_layout 1024

let parse buffer len =
  let rec aux buffer =
    if Ctypes.CArray.length buffer = 0 then []
    else (
      assert (Ctypes.CArray.length buffer >= Ctypes.sizeof C.Types.Drm_event.t);
      let hdr = !@ (Ctypes.from_voidp C.Types.Drm_event.t (Ctypes.to_voidp buffer.astart)) in
      let event_type = Ctypes.getf hdr C.Types.Drm_event.typ in
      let event_len = Ctypes.getf hdr C.Types.Drm_event.length in
      let event_data = Ctypes.CArray.sub buffer ~pos:0 ~length:event_len in
      let module T = C.Types.Drm_event_type in
      let event =
        if event_type = T.vblank then Vblank (Vblank.of_c event_data)
        else if event_type = T.flip_complete then Flip_complete (Vblank.of_c event_data)
        else if event_type = T.crtc_sequence then Crtc_sequence (Crtc_sequence.of_c event_data)
        else Unknown event_type
      in
      let buffer = Ctypes.CArray.sub buffer ~pos:event_len ~length:(Ctypes.CArray.length buffer - event_len) in
      event :: aux buffer
    )
  in
  let events =
    buffer
    |> Ctypes.(array_of_bigarray array1)
    |> Ctypes.CArray.sub ~pos:0 ~length:len
    |> aux
  in
  ignore (Sys.opaque_identity buffer);   (* Ensure buffer isn't GC'd until now *)
  events
