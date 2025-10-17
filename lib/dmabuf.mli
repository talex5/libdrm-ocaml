(** Sharing buffers between processes. *)

type t = Unix.file_descr
(** A dma-buf can be used to share buffers between processes. *)

val of_handle : rw:bool -> Device.t -> Buffer.id -> t
(** Export a buffer handle as a PRIME file descriptor.

    The new FD is created as close-on-exec. *)

val to_handle : Device.t -> t -> Buffer.id
(** [to_handle dev dmabuf] is a GEM handle for the imported buffer.

    See the warning on {!Buffer.id} about duplicate handles. *)

type sync_file = Unix.file_descr
(** A Linux sync-file can be used to pass semaphores between processes. *)

val export_sync_file : t -> [`RW] -> sync_file
(** Retrieve the current set of fences on a dma-buf as a sync_file. *)

val import_sync_file : t -> sync_file_fd:sync_file -> [`RW] -> unit
(** Insert a sync_file into a dma-buf for the purposes of implicit synchronization with other dma-buf consumers. *)
