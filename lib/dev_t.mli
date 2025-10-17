(** The [dev_t] type, identifying a Unix device. *)

type t = PosixTypes.Dev.t

val v : (int * int) -> t
(** [v (major, minor)] is a dev_t for the given major:minor device. *)

val of_raw : string -> t
(** [of_raw octets] gets a [t] from a string of bytes (e.g. from Wayland). *)

val of_int64 : int64 -> t
(** [of_int64 x] gets a [t] from e.g. {!Unix.stats.st_rdev}. *)

val pp : t Fmt.t
