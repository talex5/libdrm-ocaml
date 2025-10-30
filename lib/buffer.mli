(** Memory holding pixel data. *)

type id = [`Buffer] Id.t
(** A GEM buffer handle.

    From Linux's drm.h:

    {i GEM handles are not reference-counted by the kernel. User-space is
    responsible for managing their lifetime. For example, if user-space imports
    the same memory object twice on the same DRM file description, the same GEM
    handle is returned by both imports, and user-space needs to ensure
    DRM_IOCTL_GEM_CLOSE is performed once only. The same situation can happen
    when a memory object is allocated, then exported and imported again on the
    same DRM file description. The DRM_IOCTL_MODE_GETFB2 IOCTL is an exception
    and always returns fresh new GEM handles even if an existing GEM handle
    already refers to the same memory object before the IOCTL is performed.} *)

val close : Device.t -> id -> unit
(** [close id] calls [DRM_IOCTL_GEM_CLOSE] (see warning comment on {!id}). *)

module Dumb : sig
  (** A slow and inefficient buffer in host memory for testing, etc.

      KMS dumb buffers provide a very primitive way to allocate a buffer object
      suitable for scanout and map it for software rendering. KMS dumb buffers are
      not suitable for hardware-accelerated rendering nor video decoding. KMS dumb
      buffers are not suitable to be displayed on any other device than the KMS
      device where they were allocated from. *)

  type t = {
    bpp : int;
    width : int;
    height : int;
    handle : id;
    pitch : int;
    size : int64;
  }

  val create : Device.t -> bpp:int -> (int * int) -> t
  (** [create dev ~bpp (width, height)] allocates memory for a
      [width, height] buffer with [bpp] bits per pixel. *)

  val map :
    Device.t -> t -> ('a, 'b) Bigarray.kind ->
    ('a, 'b, Bigarray.c_layout) Bigarray.Array2.t
  (** [map dev t kind] makes [t] available as an OCaml Bigarray.

      @raise Invalid_argument if [kind] is not compatible with [t.bpp]. *)

  val destroy : Device.t -> id -> unit

  val get_map_offset : Device.t -> id -> int64
  (** Wraps the low-level "drmModeCreateDumbBuffer".

     {!map} will call this for you automatically. *)

  val pp : t Fmt.t [@@ocaml.toplevel_printer]
end
