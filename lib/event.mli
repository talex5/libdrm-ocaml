(** Parsing events from a device. *)

type buffer = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

module Vblank : sig
  type t = {
    sequence : Unsigned.UInt32.t;
    tv_sec : Unsigned.UInt32.t;
    tv_usec : int;
    crtc_id : Kms.Crtc.id;
    user_data : nativeint;
  }

  val pp : t Fmt.t
end

module Crtc_sequence : sig
  type t = {
    sequence : Unsigned.UInt64.t;
    time_ns : Unsigned.UInt64.t;
    user_data : nativeint;
  }

  val pp : t Fmt.t
end

type t =
  | Vblank of Vblank.t
  | Flip_complete of Vblank.t
  | Crtc_sequence of Crtc_sequence.t
  | Unknown of Unsigned.UInt32.t * char Ctypes.CArray.t

val pp : t Fmt.t

val create_buffer : unit -> buffer
(** [create_buffer ()] creates a buffer of a suitable size. *)

val parse : buffer -> int -> t list
(** [parse buffer len cb] parses the events in the first [len] bytes of [buffer].

    Hint: you can use [let len = Unix.read_bigarray dev buffer 0 (Bigarray.Array1.dim buffer)] to
    read events into the buffer. The buffer must contain only complete events,
    but the kernel only returns complete events when you read, so this shouldn't be a problem. *)
