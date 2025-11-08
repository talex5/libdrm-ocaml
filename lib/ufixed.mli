type t = private Unsigned.UInt32.t
(** A 16.16 fixed-point value. *)

val of_int : int -> t

val of_float : float -> t
val to_float : t -> float

val of_bits : Unsigned.UInt32.t -> t
val to_bits : t -> Unsigned.UInt32.t

val pp : t Fmt.t [@@ocaml.toplevel_printer]
