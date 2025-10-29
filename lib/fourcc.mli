(** Pixel formats *)

type t = private int32

val to_string : t -> string
(** [to_string t] is the four-character string for [t]. *)

val of_string : string -> t

val of_int32 : int32 -> t
val of_uint32 : Unsigned.UInt32.t -> t
val to_uint32 : t -> Unsigned.UInt32.t

val pp : t Fmt.t [@@ocaml.toplevel_printer]

val xr24 : t
