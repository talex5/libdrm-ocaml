(** Kernel mode-setting.

   This API is used to configure physical monitors (screen resolution, framebuffers, etc).

   See the Linux {{: https://www.kernel.org/doc/html/latest/gpu/drm-kms.html}Kernel Mode Setting} documentation for more information.  *)

type uint32 := Unsigned.UInt32.t

module type BITSET = sig
  type t = private uint32

  val empty : t
  (** [empty] is the empty set (i.e. 0). *)

  val ( + ) : t -> t -> t
  (** [a + b] is the union of [a] and [b] (i.e. [logor]). *)

  val mem : t -> t -> bool
  (** [mem flag x] checks if [flag] is set in [x].
      (i.e. if [x land flag = flag]) *)

  val of_uint32 : uint32 -> t

  val pp : t Fmt.t [@@ocaml.toplevel_printer]
end

module Rect : sig
  type t = {
    x1 : int; y1 : int;
    x2 : int; y2 : int;
  }

  val pp : Format.formatter -> t -> unit
end

module Mode_info : sig
  (** Screen mode resolution and timing information. *)

  module Type : sig
    include BITSET

    val builtin : t
    val clock_c : t
    val crtc_c : t
    val preferred : t
    val default : t
    val userdef : t
    val driver : t
  end

  module Flags : sig
    include BITSET

    val phsync : t
    val nhsync : t
    val pvsync : t
    val nvsync : t
    val interlace : t
    val dblscan : t
    val csync : t
    val pcsync : t
    val ncsync : t
    val hskew : t
    val bcast : t
    val pixmux : t
    val dblclk : t
    val clkdiv2 : t
  end

  module Stereo_mode : sig
    type t =
      | None
      | Frame_packing
      | Field_alternative
      | Line_alternative
      | Side_by_side_full
      | L_depth
      | L_depth_gfx_gfx_depth
      | Top_and_bottom
      | Side_by_side_half
      | Unknown of uint32

    val pp : t Fmt.t [@@ocaml.toplevel_printer]
  end

  module Aspect_ratio : sig
    type t =
      | R_none
      | R_4_3
      | R_16_9
      | R_64_27
      | R_256_135
      | Unknown of uint32

    val pp : t Fmt.t [@@ocaml.toplevel_printer]
  end

  type t = {
    clock : int;
    hdisplay : int;
    hsync_start : int;
    hsync_end : int;
    htotal : int;
    hskew : int;
    vdisplay : int;
    vsync_start : int;
    vsync_end : int;
    vtotal : int;
    vscan : int;
    vrefresh : int;
    flags : Flags.t;
    stereo_mode : Stereo_mode.t;
    aspect_ratio : Aspect_ratio.t;
    typ : Type.t;
    name : string;
  }

  val vrefresh : t -> float

  val pp : t Fmt.t [@@ocaml.toplevel_printer]

  val pp_summary : t Fmt.t
  (** e.g. "3840x2160 60.00Hz" *)
end

module Sub_pixel : sig
  (** How pixels are physically arranged on a particular monitor. *)

  type t =
    | Unknown
    | Horizontal_rgb
    | Horizontal_bgr
    | Vertical_rgb
    | Vertical_bgr
    | None

  val pp : t Fmt.t
end

module Blob : sig
  (** An untyped string of bytes. *)

  type id = [`Blob] Id.t
  type t = { id : id; data : string }

  val get : Device.t -> id -> t option

  val pp : t Fmt.t
end

module Property : sig
  (** A property can be used to read or write extra settings on an object (resource). *)

  type id = [`Property] Id.t
  (** Properties have numeric IDs and names. Only the names are standarised. *)

  type raw_value = Unsigned.UInt64.t

  module Info : sig
    (** Metadata describing properies. *)

    module Named_value : sig
      type t = { name : string; value : raw_value }
      (** A possible value for an enumerated type. *)

      val pp : t Fmt.t
    end

    type ty =
      | Unsigned_range of int64 * int64
      | Signed_range of int64 * int64
      | Enum of Named_value.t list
      | Blob of Blob.id list
      | Bitmask of Named_value.t list
      | Object
      | Unknown of uint32

    type t = { prop_id : id; name : string; ty : ty }

    val get : Device.t -> id -> t

    val pp : t Fmt.t
  end

  type ('obj, 'value) t
  (** A [('obj, 'value) t] is a property on objects of type ['obj] with values of type ['value]. *)

  val create :
    read:(Info.t -> raw_value -> 'v) ->
    write:(Info.t -> 'v -> raw_value) ->
    string -> (_, 'v) t
  (** [create ~read ~write name] defines a new property.

      This is useful if you need a property that isn't pre-defined. *)

  val create_bool : string -> (_, bool) t
  (** [create_bool name] is a property whose value is 0 or 1/non-zero for true. *)

  val create_int : string -> (_, int) t

  val create_fixed : string -> (_, Ufixed.t) t

  val create_id : string -> (_, _ Id.t) t
  (** [create_id name] is a property whose value is an object ID. *)

  val create_id_opt : string -> (_, (_ Id.t option)) t
  (** [create_id_opt name] is a property whose value is an optional ID. *)

  val create_enum :
    string ->
    (string * ([> `Unknown of raw_value] as 'a)) list ->
    (_, 'a) t
  (** [create_enum name values] exposes an enum property using an OCaml variant type. *)

  module Map : Map.S with type key = id
end

module Properties : sig
  (** Drivers can add extra properties to objects, discoverable at runtime. *)

  module Type : sig
    type _ t =
      | Crtc : [`Crtc] t
      | Connector : [`Connector] t
      | Encoder : [`Encoder] t
      | Mode : [`Mode] t
      | Property : [`Property] t
      | Fb : [`Fb] t
      | Blob : [`Blob] t
      | Plane : [`Plane] t
      | Any : _ t

    val pp : _ t Fmt.t
  end

  type 'a metadata
  (** Metadata about extra properties available on an object. *)

  val object_id : 'a metadata -> 'a Id.t

  val lookup_property : 'a metadata -> ('a, _) Property.t -> Property.Info.t option
  (** [lookup_name metadata p] returns information about the property [p]. *)

  val set_value : Device.t -> 'a metadata -> ('a, 'v) Property.t -> 'v -> unit
  (** [set_value dev t p v] sets the value of [p] to [v] on the object [t]. *)

  module Values : sig
    type 'a t = private {
      metadata : 'a metadata;
      values : Unsigned.UInt64.t Property.Map.t;
    }

    val get : Device.t -> 'a Type.t -> 'a Id.t -> 'a t
    (** [get dev ty id] gets the properties (values and types) for object [id] (of type [ty]).

        Use [Drm.Client_cap.(set atomic) dev true] before calling this to get all the properties. *)

    val get_value : 'a t -> ('a, 'v) Property.t -> 'v option
    (** [get_value t p] gets the value of [p] (retrieved at the time of the {!get}). *)

    val get_value_exn : 'a t -> ('a, 'v) Property.t -> 'v

    val pp : _ t Fmt.t [@@ocaml.toplevel_printer]

    (** {2 Low-level API} *)

    type binding = Property.id * Property.raw_value

    type raw = binding list

    val pp_binding : binding Fmt.t [@@ocaml.toplevel_printer]

    val get_raw : Device.t -> 'a Type.t -> 'a Id.t -> raw
    (** [get_raw dev id ty] returns the raw (id, value) pairs without getting the metadata.

        This isn't very useful, because the IDs aren't standardised, so you usually need the names too. *)

    val of_raw : Device.t -> 'a Type.t -> 'a Id.t -> raw -> 'a t
  end
end

module Connector : sig
  (** A physical connector used to attach a monitor. *)

  module Connection : sig
    (** User-space should first try to enable [Connected] connectors and
        ignore other connectors. If there are no [Connected] connectors,
        user-space should then try to probe and enable [Unknown_connection]
        connectors. *)

    type t =
      | Connected               (** The connector has a sink plugged in *)
      | Disconnected
      | Unknown_connection

    val pp : t Fmt.t
  end

  module Type : sig
    type t =
      | Unknown
      | VGA
      | DVII
      | DVID
      | DVIA
      | Composite
      | SVIDEO
      | LVDS
      | Component
      | NinePinDIN
      | DisplayPort
      | HDMIA
      | HDMIB
      | TV
      | EDP
      | VIRTUAL
      | DSI
      | DPI
      | WRITEBACK
      | SPI
      | USB

    val pp : t Fmt.t

    val name : t -> string
    (** A short name for users. e.g. [DisplayPort] is "DP" *)
  end

  type id = [`Connector] Id.t

  type t = {
    connector_id : id;
    encoder_id : [`Encoder] Id.t option;
    connector_type : Type.t;
    connector_type_id : int;
    connection : Connection.t;
    mm_width : int;
    mm_height : int;
    subpixel : Sub_pixel.t;
    modes : Mode_info.t list;
    props : Properties.Values.raw;
    encoders : [`Encoder] Id.t list;
  }

  val get : Device.t -> id -> t
  (** Retrieve all information about the connector. This will do
      a forced probe on the connector to retrieve remote information such as
      EDIDs from the display device. *)

  val get_current : Device.t -> id -> t
  (** Retrieve current information, i.e the currently active mode and
      encoder, about the connector. This will not do any probing on the
      connector or remote device, and only reports what is currently known.
      For the complete set of modes and encoders associated with the connector
      use {!get} which will do a probe to determine any display link changes
      first. *)

  val id : t -> id

  val pp_modes : Mode_info.t list Fmt.t [@@ocaml.toplevel_printer]

  val pp_name : t Fmt.t
  (** e.g. "DP-1" *)

  val pp : t Fmt.t [@@ocaml.toplevel_printer]

  (** {2 Properties} *)

  type 'a property = ([`Connector], 'a) Property.t
  val get_properties : Device.t -> id -> [`Connector] Properties.Values.t

  val crtc_id : [`Crtc] Id.t option property
end

module Fb : sig
  (** A framebuffer manages the inputs to a {!Crtc}. *)

  module Plane : sig
    type 'handle t = { handle : 'handle; pitch : int; offset : int }

    val v : pitch:int -> ?offset:int -> Buffer.id -> Buffer.id t

    val pp_opt : Buffer.id option t Fmt.t
  end

  type id = [`Fb] Id.t

  type t = {
    fb_id : id;
    width : int;
    height : int;
    pixel_format : Fourcc.t;
    modifier : Modifier.t option;
    interlaced : bool;
    planes : Buffer.id option Plane.t list;
  }

  val get : Device.t -> id -> t
  (** If the client is DRM master or has CAP_SYS_ADMIN, {!Plane.handle} fields
      will be filled with GEM buffer handles. Fresh new GEM handles are always
      returned, even if another GEM handle referring to the same memory object
      already exists on the DRM file description. The caller is responsible for
      removing the new handles, e.g. via {!close_plane_handles}. The same new handle
      will be returned for multiple planes in case they use the same memory
      object.

      Otherwise, all handles will be [None].

      To obtain DMA-BUF FDs for each plane without leaking GEM handles, user-space
      can export each handle via {!Dmabuf.of_handle}, then immediately
      close each unique handle via {!close_plane_handles}. *)

  val id : t -> id

  val add :
    ?interlaced:bool ->
    ?modifier:Modifier.t ->
    Device.t ->
    size:(int * int) ->
    pixel_format:Fourcc.t ->
    planes:Buffer.id Plane.t list ->
    id

  val dirty : Device.t -> id -> Rect.t list -> unit
  (** Flush out the damaged area supplied as a clip rectangle list.

      Code that does frontbuffer rendering must call this to flush out the
      changes on manual-update display outputs, e.g. usb display-link, mipi
      manual update panels or edp panel self refresh modes. *)

  val close_plane_handles : Device.t -> t -> unit
  (** Close each unique GEM handle in {!t.planes}. *)

  val rm : Device.t -> id -> unit
  (** This removes a framebuffer previously added via {!add}.

      Warning: removing a framebuffer currently in-use on an enabled plane will
      disable that plane. The CRTC the plane is linked to may also be disabled
      (depending on driver capabilities). *)

  val close : Device.t -> id -> unit
  (** Like {!rm}, except it doesn't disable planes and CRTCs. As long as the
      framebuffer is used by a plane, it's kept alive. When the plane no longer
      uses the framebuffer (because the framebuffer is replaced with another
      one, or the plane is disabled), the framebuffer is cleaned up.

      This is useful to implement flicker-free transitions between two
      processes.

      Depending on the threat model, user-space may want to ensure that the
      framebuffer doesn’t expose any sensitive user information: closed
      framebuffers attached to a plane can be read back by the next DRM master. *)

  val pp : t Fmt.t [@@ocaml.toplevel_printer]
end

module Crtc : sig
  (** A CRT Controller.

      Typically, one CRTC is used for each monitor,
      although it is possible to drive two identical monitors in mirror mode with only one. *)

  type id = [`Crtc] Id.t

  type t = {
    crtc_id : id;
    fb_id : Fb.id option;
    x : int;            (** Position on the framebuffer *)
    y : int;
    width : int;
    height : int;
    mode : Mode_info.t option;
    gamma_size : int;   (** Number of gamma stops *)
  }

  val get : Device.t -> id -> t

  val id : t -> id

  val set : Device.t -> id -> ?fb:Fb.id -> pos:int * int -> connectors:[`Connector] Id.t list -> Mode_info.t option -> unit
  (** The old non-atomic API. *)

  val page_flip :
    ?event:nativeint ->
    ?async:bool ->
    ?target:([`None | `Absolute of Unsigned.UInt32.t | `Relative of int]) ->
    Device.t -> id ->
    Fb.id -> unit
  (** [page_flip dev id fb] asks KMS to schedule a page flip for CRTC [id].

      Once any pending rendering targeting [fb] (as of ioctl time) has
      completed, the CRTC will be reprogrammed to display [fb] after the next
      vertical refresh. The call returns immediately, but subsequent rendering
      to the current fb will block in the execbuffer ioctl until the page flip
      happens. If a page flip is already pending as the ioctl is called,
      {!Unix.EBUSY} will be raised.

      @param event Requests that drm sends back an event when the page
                   flip is done. The event data will be returned as the
                   [user_data] argument for {!Event.vblank_handler}.

      @param async Requests that the flip happen 'as soon as possible', meaning
                   that it not delay waiting for vblank. This may cause tearing
                   on the screen.

      @param target If [`Absolute seq], [seq] denotes the absolute vblank
                    sequence when the flip should take effect.
                    If [`Relative seq], [seq] denotes the relative (to the
                    current one when the ioctl is called) vblank sequence when
                    the flip should take effect ([seq] must be 0 or 1).
                    [`None] is the same as [`Relative 1], unless [async] is true,
                    in which case it behaves as [`Relative 0].

                    [?target] is merely to clarify the target for when code
                    dealing with a page flip runs during a vertical blank period. *)

  val queue_sequence :
    ?next_on_miss:bool ->
    user_data:nativeint ->
    Device.t -> id -> [`Absolute of Unsigned.UInt64.t | `Relative of int] ->
    Unsigned.UInt64.t
  (** Queue an event to be delivered at the specified sequence. The timestamp
      marks when the first pixel of the refresh cycle leaves the display engine
      for the display.

      @param next_on_miss Use next sequence if we've missed. *)

  val set_cursor : Device.t -> id -> ?hot:(int * int) -> size:(int * int) -> Buffer.id option -> unit
  val move_cursor : Device.t -> id -> int * int -> unit

  type gamma_lut = (int, Bigarray.int16_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

  val get_gamma : Device.t -> t -> gamma_lut * gamma_lut * gamma_lut
  (** Get the gamma table. *)

  val set_gamma : Device.t -> t -> gamma_lut * gamma_lut * gamma_lut -> unit
  (** Set the gamma table. *)

  val pp : t Fmt.t [@@ocaml.toplevel_printer]

  (** {2 Properties} *)

  type 'a property = ([`Crtc], 'a) Property.t
  val get_properties : Device.t -> id -> [`Crtc] Properties.Values.t

  val active : bool property
end

module Plane : sig
  (** A plane wraps a {!Buffer} with some extra metadata. *)

  type id = [`Plane] Id.t

  type t = {
    formats : Fourcc.t list;
    plane_id : id;
    crtc_id : Crtc.id option;
    fb_id : Fb.id option;
    crtc_x : int;
    crtc_y : int;
    x : int;
    y : int;
    possible_crtcs : int;
  }

  val list : Device.t -> id list
  (** Get the plane resources.

      Note: You must enable {!Client_cap.atomic} mode first. *)

  val get : Device.t -> id -> t

  val id : t -> id

  type 'a region = { x : 'a; y : 'a; w : 'a; h : 'a }

  val set : Device.t -> id -> crtc:Crtc.id -> fb:Fb.id -> src:Ufixed.t region -> dst:int region -> unit
  (** [set dev id ~crtc ~fb ~src ~dst] sets plane [id] to show [fb] region [src] on [crtc] at [dst]. *)

  val pp : t Fmt.t [@@ocaml.toplevel_printer]

  (** {2 Properties} *)

  type 'a property = ([`Plane], 'a) Property.t
  val get_properties : Device.t -> id -> [`Plane] Properties.Values.t

  val typ : [`Cursor | `Overlay | `Primary | `Unknown of Property.raw_value] property
  val fb_id : [`Fb] Id.t option property
  val crtc_id : [`Crtc] Id.t option property

  val crtc_x : int property
  val crtc_y : int property
  val crtc_w : int property
  val crtc_h : int property

  val src_x : Ufixed.t property
  val src_y : Ufixed.t property
  val src_w : Ufixed.t property
  val src_h : Ufixed.t property

  val in_formats : Device.t -> (Fourcc.t * Modifier.t) list property

  val in_fence_fd : Unix.file_descr option property
end

module Encoder : sig
  (** Encoders reads pixel data from a {!Crtc} and output it in a suitable format for a {!Connector}.

      The Linux documentation says that exposing encoders to user-space was a design mistake.
      With the newer atomic API, you can use {!Connector.crtc_id} to get the CRTC directly. *)

  module Type : sig
    type t =
      | NONE
      | DAC
      | TMDS
      | LVDS
      | TVDAC
      | VIRTUAL
      | DSI
      | DPMST
      | DPI
      | Unknown of uint32

    val pp : t Fmt.t
  end

  type id = [`Encoder] Id.t

  type t = {
    encoder_id : id;
    encoder_type : Type.t;
    crtc_id : Crtc.id option;
    possible_crtcs : int;
    possible_clones : int;
  }

  val get : Device.t -> id -> t

  val id : t -> id

  val pp : t Fmt.t [@@ocaml.toplevel_printer]
end

module Resources : sig
  (** The set of resources provided by the hardware (excluding planes; see {!Plane.list}). *)

  type t = {
    fbs : Fb.id list;
    crtcs : Crtc.id list;
    connectors : Connector.id list;
    encoders : Encoder.id list;
    min_width : int;
    max_width : int;
    min_height : int;
    max_height : int;
  }
  val get : Device.t -> t
  val pp : t Fmt.t [@@ocaml.toplevel_printer]
end

module Atomic_req : sig
  (** Atomic requests. *)

  type t
  (** A dynamic array of (object_id, property_id, value) items. *)

  val create : unit -> t
  (** [create ()] is a fresh empty array. *)

  val add_property : t -> 'a Properties.metadata -> ('a, 'v) Property.t -> 'v -> unit
  (** [add_property t meta prop value] appends one item to [t]. *)

  val commit :
    ?page_flip_event:nativeint ->
    ?page_flip_async:bool ->
    ?test_only:bool ->
    ?nonblock:bool ->
    ?allow_modeset:bool ->
    Device.t -> t -> unit
  (** [commit dev t] atomically applies all the changes in [t]. *)

  val duplicate : t -> t
  (** [duplicate t] is a copy of [t]. *)
    
  val merge : t -> t -> unit
  (** [merge t arg] appends a copy of each element of [arg] to [t]. *)

  val get_cursor : t -> int
  (** [get_cursor t] is the number of allocated items in [t]. *)
end
