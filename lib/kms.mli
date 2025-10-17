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

  val pp : t Fmt.t

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

  val create_id_opt : string -> (_, (_ Id.t option)) t
  (** [create_id_opt name] is a property whose value is an optional ID. *)

  val create_enum :
    string ->
    (string * ([> `Unknown of raw_value] as 'a)) list ->
    (_, 'a) t
  (** [create_enum name values] exposes an enum property using an OCaml variant type. *)
end

module Properties : sig
  (** A set of properties attached to an object. *)

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

  type 'a t

  type object_types = [ `Blob | `Connector | `Crtc | `Encoder | `Fb | `Mode | `Plane | `Property ]

  val get : Device.t -> 'a Type.t -> ([< object_types] as 'a) Id.t -> 'a t
  (** [get dev ty id] gets the propeties for object [id] (of type [ty]).

      Use [Drm.Client_cap.(set atomic) dev true] before calling this to get all the properties. *)

  val object_id : 'a t -> 'a Id.t

  val get_value : 'a t -> ('a, 'v) Property.t -> 'v option
  (** [get_value t p] gets the value of [p] (retrieved at the time of the {!get}). *)

  val get_value_exn : 'a t -> ('a, 'v) Property.t -> 'v

  val get_info : _ t -> string -> Property.Info.t option
  (** [get_info t name] gets the metadata for [name]. *)

  val pp : _ t Fmt.t [@@ocaml.toplevel_printer]

  (** {2 Low-level API} *)

  type binding = Property.id * Property.raw_value

  type raw_properties = binding list

  val pp_binding : binding Fmt.t [@@ocaml.toplevel_printer]

  val get_raw : Device.t -> 'a Type.t -> ([< object_types] as 'a) Id.t -> raw_properties
  (** [get_raw dev id ty] returns the raw (id, value) pairs without getting the metadata.

      This isn't very useful, because the IDs aren't standardised, so you usually need the names too. *)

  val of_raw : Device.t -> 'a Id.t -> raw_properties -> 'a t
end

module Connector : sig
  (** A physical connector used to attach a monitor. *)

  module Connection : sig
    type t =
      | Connected
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
    props : Properties.raw_properties;
    encoders : [`Encoder] Id.t list;
  }

  val get : Device.t -> id -> t

  val pp_modes : Mode_info.t list Fmt.t [@@ocaml.toplevel_printer]
  val pp_name : t Fmt.t
  val pp : t Fmt.t [@@ocaml.toplevel_printer]

  (** {2 Properties} *)

  type 'a property = ([`Connector], 'a) Property.t
  val get_properties : Device.t -> id -> [`Connector] Properties.t

  val crtc_id : [`Crtc] Id.t option property
end

module Fb : sig
  (** A framebuffer manages the inputs to a {!Crtc}. *)

  module Plane : sig
    type 'handle t = { handle : 'handle; pitch : int; offset : int }
    val pp : Buffer.id option t Fmt.t
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

  val add :
    ?interlaced:bool ->
    ?modifier:Modifier.t ->
    Device.t ->
    size:(int * int) ->
    pixel_format:Fourcc.t ->
    planes:Buffer.id Plane.t list ->
    id

  val close_plane_handles : Device.t -> t -> unit
  (** Close each unique GEM handle in {!t.planes}. *)

  val rm : Device.t -> id -> unit
  (** This removes a framebuffer previously added via {!add}.

      Warning: removing a framebuffer currently in-use on an enabled plane will
      disable that plane. The CRTC the plane is linked to may also be disabled
      (depending on driver capabilities). *)

  val pp : t Fmt.t [@@ocaml.toplevel_printer]

  (** {2 Properties} *)

  type 'a property = ([`Fb], 'a) Property.t
  val get_properties : Device.t -> id -> [`Fb] Properties.t
end

module Crtc : sig
  (** A CRT Controller.

      Typically, one CRTC is used for each monitor,
      although it is possible to drive two identical monitors in mirror mode with only one. *)

  type id = [`Crtc] Id.t

  type t = {
    crtc_id : id;
    buffer_id : Fb.id option;
    x : int;
    y : int;
    width : int;
    height : int;
    mode : Mode_info.t option;
    gamma_size : int;
  }

  val get : Device.t -> id -> t

  val set : Device.t -> id -> ?buffer:Fb.id -> pos:int * int -> connectors:[`Connector] Id.t list -> Mode_info.t option -> unit
  (** The old non-atomic API. *)

  val page_flip : ?event:bool -> Device.t -> id -> user_data:unit Ctypes_static.ptr -> Fb.id -> unit

  val set_cursor : Device.t -> id -> Buffer.id option -> size:(int * int) -> unit
  val move_cursor : Device.t -> id -> int * int -> unit

  val pp : t Fmt.t [@@ocaml.toplevel_printer]

  (** {2 Properties} *)

  type 'a property = ([`Crtc], 'a) Property.t
  val get_properties : Device.t -> id -> [`Crtc] Properties.t
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

  val get : Device.t -> id -> t

  val pp : t Fmt.t [@@ocaml.toplevel_printer]

  (** {2 Properties} *)

  type 'a property = ([`Plane], 'a) Property.t
  val get_properties : Device.t -> id -> [`Plane] Properties.t

  val typ : [`Cursor | `Overlay | `Primary | `Unknown of Property.raw_value] property
  val fb_id : [`Fb] Id.t option property
  val crtc_id : [`Crtc] Id.t option property
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

  val pp : t Fmt.t [@@ocaml.toplevel_printer]

  (** {2 Properties} *)

  type 'a property = ([`Encoder], 'a) Property.t
  val get_properties : Device.t -> id -> [`Encoder] Properties.t
end

module Resources : sig
  (** The set of resources provided by the hardware (excluding {!Plane_resources}). *)

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

module Plane_resources : sig
  (** The set of planes provided by the hardware. *)

  type t = Plane.id list

  val get : Device.t -> t
  (** Enable {!Client_cap.atomic} mode first to get the planes. *)

  val pp : t Fmt.t
end

module Atomic_req : sig
  (** Atomic requests. *)

  type t

  val create : unit -> t

  val add_property : t -> [< Properties.object_types ] Properties.t -> ('a, 'v) Property.t -> 'v -> unit

  val add_property_full : t -> [< Properties.object_types ] Properties.t -> ('a, 'v) Property.t -> 'v -> int

  val commit :
    ?page_flip_event : bool ->
    ?page_flip_async : bool ->
    ?test_only : bool ->
    ?nonblock : bool ->
    ?allow_modeset : bool ->
    Device.t -> t -> unit
end
