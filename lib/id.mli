(** Integer IDs with unique types to help prevent mistakes. *)

type +'a t = private int

val of_int : int -> 'a t
val of_uint32 : Unsigned.UInt32.t -> 'a t
val of_uint64 : Unsigned.UInt64.t -> 'a t

val of_uint32_opt : Unsigned.UInt32.t -> 'a t option
(** Returns [None] if the handle is 0, or [Some id] otherwise. *)

val of_uint64_opt : Unsigned.UInt64.t -> 'a t option

val of_int64 : int64 -> 'a t

val to_uint32 : _ t -> Unsigned.UInt32.t
val to_uint64 : _ t -> Unsigned.UInt64.t

val to_uint64_opt : _ t option -> Unsigned.UInt64.t

val pp : _ t Fmt.t
