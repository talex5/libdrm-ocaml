(** Parsing events from a device. *)

type buffer = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

type vblank_handler =
  sequence:Unsigned.UInt32.t ->
  tv_sec:Unsigned.UInt32.t ->
  tv_usec:int ->
  crtc_id:Kms.Crtc.id ->
  user_data:nativeint ->
  unit

type sequence_handler =
  sequence:Unsigned.UInt64.t ->
  time_ns:Unsigned.UInt64.t ->
  user_data:nativeint ->
  unit

val create_buffer : unit -> buffer
(** [create_buffer ()] creates a buffer of a suitable size. *)

val parse :
  ?vblank:vblank_handler ->
  ?flip_complete:vblank_handler ->
  ?sequence_handler:sequence_handler ->
  ?unknown_event:(Unsigned.UInt32.t -> unit) ->
  buffer -> int -> unit
(** [parse buffer len] iterates through the events in [buffer], calling the appropriate handlers.

    If no handler is given for an event then the event is skipped.

    Hint: you can use [let len = Unix.read_bigarray dev buffer 0 (Bigarray.Array1.dim buffer)] to
    read events into the buffer. The buffer must contain only complete events,
    but the kernel only returns complete events when you read, so this shouldn't be a problem. *)
