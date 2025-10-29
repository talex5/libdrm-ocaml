(** Format modifiers. *)

type t = private int64

val of_int64 : int64 -> t
val of_uint64 : Unsigned.UInt64.t -> t
val to_uint64 : t -> Unsigned.UInt64.t

val pp : t Fmt.t [@@ocaml.toplevel_printer]

val reserved : t

val linear: t
(** Just plain linear layout. Note that this is different from not specifying any
    modifier, which tells the driver to also take driver-internal information
    into account and so might actually result in a tiled framebuffer. *)
