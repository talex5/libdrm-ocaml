(** Features offered by the Linux device driver to the application. *)

type 'a t

val get : 'a t -> Device.t -> ('a, Unix.error) result
val get_exn : 'a t -> Device.t -> 'a

(** {2 Features} *)

val dumb_buffer : bool t
(** The driver supports creating dumb buffers (via {!Buffer.Dumb.create}). *)

val vblank_high_crtc : bool t
(** The kernel supports specifying a CRTC index in the high bits of
    drm_wait_vblank_request.type.

    Starting kernel version 2.6.39, this capability is always set to true *)

val dumb_preferred_depth : int t
(** The preferred bit depth for dumb buffers.

    The bit depth is the number of bits used to indicate the color of a single
    pixel excluding any padding. This is different from the number of bits per
    pixel. For instance, XRGB8888 has a bit depth of 24 but has 32 bits per
    pixel.

    Note that this preference only applies to dumb buffers, it's irrelevant for
    other types of buffers. *)

val dumb_prefer_shadow : bool t
(** The driver prefers userspace to render to a shadow buffer
    instead of directly rendering to a dumb buffer. For best speed, userspace
    should do streaming ordered memory copies into the dumb buffer and never
    read from it.

    Note that this preference only applies to dumb buffers, it's irrelevant for
    other types of buffers. *)

val prime : int t
(** Bitfield of supported PRIME sharing capabilities.

    Starting from Linux version 6.6, both DRM_PRIME_CAP_IMPORT (1) and
    DRM_PRIME_CAP_EXPORT (2) are always advertised.

    PRIME buffers are exposed as dma-buf file descriptors.
    See {!Dmabuf}. *)

val timestamp_monotonic : bool t
(** If set to false, the kernel will report timestamps with CLOCK_REALTIME in
    struct drm_event_vblank. If set to true, the kernel will report timestamps with
    CLOCK_MONOTONIC. See clock_gettime(2) for the definition of these clocks.

    Starting from kernel version 2.6.39, the default value for this capability
    is true. Starting kernel version 4.15, this capability is always set to true. *)

val async_page_flip : bool t
(** If set to true, the driver supports DRM_MODE_PAGE_FLIP_ASYNC for legacy page-flips. *)

val cursor_width : int t
(** The [cursor_width] and [cursor_height] capabilities return a valid
    width x height combination for the hardware cursor. The intention is that a
    hardware agnostic userspace can query a cursor plane size to use.

    Note that the cross-driver contract is to merely return a valid size;
    drivers are free to attach another meaning on top, eg. i915 returns the
    maximum plane size.  *)

val cursor_height : int t

val addfb2_modifiers : bool t
(** If set to true, the driver supports supplying modifiers in {!Kms.Fb.add}. *)

val page_flip_target : bool t
(** If set to true, the driver supports the DRM_MODE_PAGE_FLIP_TARGET_ABSOLUTE and
    DRM_MODE_PAGE_FLIP_TARGET_RELATIVE flags in
    drm_mode_crtc_page_flip_target.flags for the DRM_IOCTL_MODE_PAGE_FLIP ioctl.  *)

val crtc_in_vblank_event : bool t
(** If set to true, the kernel supports reporting the CRTC ID in
    drm_event_vblank.crtc_id for the DRM_EVENT_VBLANK and
    DRM_EVENT_FLIP_COMPLETE events.

    Starting kernel version 4.12, this capability is always set to true.  *)

val syncobj : bool t
(** If set to true, the driver supports sync objects. *)

val syncobj_timeline : bool t
(** If set to true, the driver supports timeline operations on sync objects. *)

val atomic_async_page_flip : bool t
(** If set to true, the driver supports DRM_MODE_PAGE_FLIP_ASYNC for atomic commits. *)
