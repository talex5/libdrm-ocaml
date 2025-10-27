open Ctypes

module Types (F : TYPE) = struct
  open F

  let int_of_unix (fd : Unix.file_descr) : int = Obj.magic fd
  let unix_of_int (fd : int) : Unix.file_descr = assert (fd >= 0); Obj.magic fd

  let fd = view ~read:unix_of_int ~write:int_of_unix int

  let id () = view ~read:Id.of_uint32 ~write:Id.to_uint32 uint32_t
  let object_id : [ `Crtc | `Connector | `Encoder | `Mode | `Property | `Fb | `Blob | `Plane ] Id.t typ = id ()
  let property_id : [`Property] Id.t typ = id ()
  let buffer_id : [`Buffer] Id.t typ = id ()
  let crtc_id : [`Crtc] Id.t typ = id ()
  let fb_id : [`Fb] Id.t typ = id ()
  let connector_id : [`Connector] Id.t typ = id ()
  let encoder_id : [`Encoder] Id.t typ = id ()
  let plane_id : [`Plane] Id.t typ = id ()
  let blob_id : [`Blob] Id.t typ = id ()

  let id_opt () =
    view uint32_t
      ~read:(fun x -> if x = Unsigned.UInt32.zero then None else Some (Id.of_uint32 x))
      ~write:(function None -> Unsigned.UInt32.zero | Some x -> Id.to_uint32 x)
  let buffer_id_opt : [`Buffer] Id.t option typ = id_opt ()
  let fb_id_opt : [`Fb] Id.t option typ = id_opt ()
  let encoder_id_opt : [`Encoder] Id.t option typ = id_opt ()
  let crtc_id_opt : [`Crtc] Id.t option typ = id_opt ()

  let flags32 = uint32_t

  let int_uint8 = view ~read:Unsigned.UInt8.to_int ~write:Unsigned.UInt8.of_int uint8_t
  let int_uint16 = view ~read:Unsigned.UInt16.to_int ~write:Unsigned.UInt16.of_int uint16_t

  (* On 64-bit systems, we can always represent 32-bit C ints as plain OCaml ints.
     On 32-bit systems, most values should also fit in a 31-bit OCaml int. *)
  let int_uint32 = view ~read:Unsigned.UInt32.to_int ~write:Unsigned.UInt32.of_int uint32_t
  let dim = int_uint32
  let pitch = int_uint32
  let offset = int_uint32

  let int_uint64 = view ~read:Unsigned.UInt64.to_int ~write:Unsigned.UInt64.of_int uint64_t

  let size = view ~read:Unsigned.UInt64.to_int64 ~write:Unsigned.UInt64.of_int64 uint64_t

  let drm_format = view uint32_t ~read:Fourcc.of_uint32 ~write:Fourcc.to_uint32
  let drm_modifier = view uint64_t ~read:Modifier.of_uint64 ~write:Modifier.to_uint64

  module Node = struct
    let primary = constant "DRM_NODE_PRIMARY" int
    let render  = constant "DRM_NODE_RENDER" int
  end

  module PageFlipFlags = struct
    let event = constant "DRM_MODE_PAGE_FLIP_EVENT" uint32_t
    let async = constant "DRM_MODE_PAGE_FLIP_ASYNC" uint32_t
  end

  module FbFlags = struct
    let interlaced = constant "DRM_MODE_FB_INTERLACED" uint32_t
    let modifiers = constant "DRM_MODE_FB_MODIFIERS" uint32_t
  end

  module Dmabuf = struct
    let rdwr = constant "DRM_RDWR" flags32
    let cloexec = constant "DRM_CLOEXEC" flags32

    let sync_rw = constant "DMA_BUF_SYNC_RW" uint32_t
    let ioctl_export_sync_file = constant "DMA_BUF_IOCTL_EXPORT_SYNC_FILE" ulong
    let ioctl_import_sync_file = constant "DMA_BUF_IOCTL_IMPORT_SYNC_FILE" ulong
  end

  module DrmPciBusInfo = struct
    type mark
    type ctype = mark Ctypes.structure
    let t : ctype F.typ = F.structure "_drmPciBusInfo"

    let domain = F.field t "domain" int_uint16
    let bus = F.field t "bus" int_uint8
    let dev = F.field t "dev" int_uint8
    let func = F.field t "func" int_uint8
    let () = F.seal t
  end

  module DrmPciDeviceInfo = struct
    type mark
    type ctype = mark Ctypes.structure
    let t : ctype F.typ = F.structure "_drmPciDeviceInfo"

    let vendor_id = F.field t "vendor_id" int_uint16
    let device_id = F.field t "device_id" int_uint16
    let subvendor_id = F.field t "subvendor_id" int_uint16
    let subdevice_id = F.field t "subdevice_id" int_uint16
    let revision_id = F.field t "revision_id" int_uint8
    let () = F.seal t
  end

  module DrmUsbBusInfo = struct
    type mark
    type ctype = mark Ctypes.structure
    let t : ctype F.typ = F.structure "_drmUsbBusInfo"

    let bus = F.field t "bus" int_uint8
    let dev = F.field t "dev" int_uint8
    let () = F.seal t
  end

  module DrmUsbDeviceInfo = struct
    type mark
    type ctype = mark Ctypes.structure
    let t : ctype F.typ = F.structure "_drmUsbDeviceInfo"

    let vendor = F.field t "vendor" int_uint16
    let product = F.field t "product" int_uint16
    let () = F.seal t
  end

  module DrmPlatformBusInfo = struct
    type mark
    type ctype = mark Ctypes.structure
    let t : ctype F.typ = F.structure "_drmPlatformBusInfo"

    let fullname = F.field t "fullname" (F.array Config.drm_platform_device_name_len F.char)
    let () = F.seal t
  end

  module DrmPlatformDeviceInfo = struct
    type mark
    type ctype = mark Ctypes.structure
    let t : ctype F.typ = F.structure "_drmPlatformDeviceInfo"

    let compatible = F.field t "compatible" F.(ptr (ptr char))
    let () = F.seal t
  end

  module DrmHost1xBusInfo = struct
    type mark
    type ctype = mark Ctypes.structure
    let t : ctype F.typ = F.structure "_drmHost1xBusInfo"

    let fullname = F.field t "fullname" (F.array Config.drm_host1x_device_name_len F.char)
    let () = F.seal t
  end

  module DrmHost1xDeviceInfo = DrmPlatformDeviceInfo

  module Businfo = struct
    type mark
    type ctype = mark Ctypes.union
    let t : ctype F.typ = F.union "{
        drmPciBusInfoPtr pci;
        drmUsbBusInfoPtr usb;
        drmPlatformBusInfoPtr platform;
        drmHost1xBusInfoPtr host1x;
    }"

    let pci = F.field t "pci" (F.ptr DrmPciBusInfo.t)
    let usb = F.field t "usb" (F.ptr DrmUsbBusInfo.t)
    let platform = F.field t "platform" (F.ptr DrmPlatformBusInfo.t)
    let host1x = F.field t "host1x" (F.ptr DrmHost1xBusInfo.t)
    let () = F.seal t
  end

  module Deviceinfo = struct
    let get_pci_revision = constant "DRM_DEVICE_GET_PCI_REVISION" uint32_t

    type mark
    type ctype = mark Ctypes.union
    let t : ctype F.typ = lift_typ (Ctypes.union "{
        drmPciDeviceInfoPtr pci;
        drmUsbDeviceInfoPtr usb;
        drmPlatformDeviceInfoPtr platform;
        drmHost1xDeviceInfoPtr host1x;
    }")

    let pci = F.field t "pci" (F.ptr DrmPciDeviceInfo.t)
    let usb = F.field t "usb" (F.ptr DrmUsbDeviceInfo.t)
    let platform = F.field t "platform" (F.ptr DrmPlatformDeviceInfo.t)
    let host1x = F.field t "host1x" (F.ptr DrmHost1xDeviceInfo.t)
    let () = F.seal t
  end

  module Device = struct
    type mark
    type t = mark Ctypes.structure
    let t : t F.typ = F.structure "_drmDevice"

    let nodes = F.field t "nodes" F.(ptr (array Config.drm_node_max (ptr char)))
    let available_nodes = F.field t "available_nodes" F.int
    let bustype = F.field t "bustype" F.int
    let businfo = F.field t "businfo" Businfo.t
    let deviceinfo = F.field t "deviceinfo" Deviceinfo.t

    let () = F.seal t
  end

  module DrmModeModeInfo = struct
    module Type = struct
      let builtin = constant "DRM_MODE_TYPE_BUILTIN" uint32_t
      let clock_c = constant "DRM_MODE_TYPE_CLOCK_C" uint32_t
      let crtc_c = constant "DRM_MODE_TYPE_CRTC_C" uint32_t
      let preferred = constant "DRM_MODE_TYPE_PREFERRED" uint32_t
      let default = constant "DRM_MODE_TYPE_DEFAULT" uint32_t
      let userdef = constant "DRM_MODE_TYPE_USERDEF" uint32_t
      let driver = constant "DRM_MODE_TYPE_DRIVER" uint32_t
    end

    module Flags = struct
      let phsync = constant "DRM_MODE_FLAG_PHSYNC" uint32_t
      let nhsync = constant "DRM_MODE_FLAG_NHSYNC" uint32_t
      let pvsync = constant "DRM_MODE_FLAG_PVSYNC" uint32_t
      let nvsync = constant "DRM_MODE_FLAG_NVSYNC" uint32_t
      let interlace = constant "DRM_MODE_FLAG_INTERLACE" uint32_t
      let dblscan = constant "DRM_MODE_FLAG_DBLSCAN" uint32_t
      let csync = constant "DRM_MODE_FLAG_CSYNC" uint32_t
      let pcsync = constant "DRM_MODE_FLAG_PCSYNC" uint32_t
      let ncsync = constant "DRM_MODE_FLAG_NCSYNC" uint32_t
      let hskew = constant "DRM_MODE_FLAG_HSKEW" uint32_t
      let bcast = constant "DRM_MODE_FLAG_BCAST" uint32_t
      let pixmux = constant "DRM_MODE_FLAG_PIXMUX" uint32_t
      let dblclk = constant "DRM_MODE_FLAG_DBLCLK" uint32_t
      let clkdiv2 = constant "DRM_MODE_FLAG_CLKDIV2" uint32_t
    end

    module Stereo_mode = struct
      let mask = constant "DRM_MODE_FLAG_3D_MASK" uint32_t

      let none = constant "DRM_MODE_FLAG_3D_NONE" uint32_t
      let frame_packing = constant "DRM_MODE_FLAG_3D_FRAME_PACKING" uint32_t
      let field_alternative = constant "DRM_MODE_FLAG_3D_FIELD_ALTERNATIVE" uint32_t
      let line_alternative = constant "DRM_MODE_FLAG_3D_LINE_ALTERNATIVE" uint32_t
      let side_by_side_full = constant "DRM_MODE_FLAG_3D_SIDE_BY_SIDE_FULL" uint32_t
      let l_depth = constant "DRM_MODE_FLAG_3D_L_DEPTH" uint32_t
      let l_depth_gfx_gfx_depth = constant "DRM_MODE_FLAG_3D_L_DEPTH_GFX_GFX_DEPTH" uint32_t
      let top_and_bottom = constant "DRM_MODE_FLAG_3D_TOP_AND_BOTTOM" uint32_t
      let side_by_side_half = constant "DRM_MODE_FLAG_3D_SIDE_BY_SIDE_HALF" uint32_t
    end

    module Aspect_ratio = struct
      let mask = constant "DRM_MODE_FLAG_PIC_AR_MASK" uint32_t

      let ar_none = constant "DRM_MODE_FLAG_PIC_AR_NONE" uint32_t
      let ar_4_3 = constant "DRM_MODE_FLAG_PIC_AR_4_3" uint32_t
      let ar_16_9 = constant "DRM_MODE_FLAG_PIC_AR_16_9" uint32_t
      let ar_64_27 = constant "DRM_MODE_FLAG_PIC_AR_64_27" uint32_t
      let ar_256_135 = constant "DRM_MODE_FLAG_PIC_AR_256_135" uint32_t
    end

    type mark
    type ctype = mark Ctypes.structure
    let t : ctype F.typ = F.structure "_drmModeModeInfo"

    let clock = F.field t "clock" int_uint32
    let hdisplay = F.field t "hdisplay" int_uint16
    let hsync_start = F.field t "hsync_start" int_uint16
    let hsync_end = F.field t "hsync_end" int_uint16
    let htotal = F.field t "htotal" int_uint16
    let hskew = F.field t "hskew" int_uint16
    let vdisplay = F.field t "vdisplay" int_uint16
    let vsync_start = F.field t "vsync_start" int_uint16
    let vsync_end = F.field t "vsync_end" int_uint16
    let vtotal = F.field t "vtotal" int_uint16
    let vscan = F.field t "vscan" int_uint16
    let vrefresh = F.field t "vrefresh" int_uint32
    let flags = F.field t "flags" F.uint32_t
    let typ = F.field t "type" F.uint32_t
    let name = F.field t "name" (F.array Config.drm_display_mode_len F.char)
    let () = F.seal t
  end

  module DrmVersion = struct
    type mark
    type ctype = mark Ctypes.structure
    let t : ctype F.typ = F.structure "_drmVersion"

    let version_major = F.field t "version_major" F.int
    let version_minor = F.field t "version_minor" F.int
    let version_patchlevel = F.field t "version_patchlevel" F.int
    let name_len = F.field t "name_len" F.int
    let name = F.field t "name" F.(ptr char)
    let date_len = F.field t "date_len" F.int
    let date = F.field t "date" F.(ptr char)
    let desc_len = F.field t "desc_len" F.int
    let desc = F.field t "desc" F.(ptr char)
    let () = F.seal t
  end

  module DrmModeRes = struct
    type mark
    type ctype = mark Ctypes.structure
    let t : ctype F.typ = F.structure "_drmModeRes"

    let count_fbs = F.field t "count_fbs" F.int
    let fbs = F.field t "fbs" (F.ptr fb_id)
    let count_crtcs = F.field t "count_crtcs" F.int
    let crtcs = F.field t "crtcs" (F.ptr crtc_id)
    let count_connectors = F.field t "count_connectors" F.int
    let connectors = F.field t "connectors" (F.ptr connector_id)
    let count_encoders = F.field t "count_encoders" F.int
    let encoders = F.field t "encoders" (F.ptr encoder_id)
    let min_width = F.field t "min_width" int_uint32
    let max_width = F.field t "max_width" int_uint32
    let min_height = F.field t "min_height" int_uint32
    let max_height = F.field t "max_height" int_uint32
    let () = F.seal t
  end

  module DrmModeCrtc = struct
    type mark
    type ctype = mark Ctypes.structure
    let t : ctype F.typ = F.structure "_drmModeCrtc"

    let crtc_id = F.field t "crtc_id" crtc_id
    let buffer_id = F.field t "buffer_id" fb_id_opt
    let x = F.field t "x" int_uint32
    let y = F.field t "y" int_uint32
    let width = F.field t "width" int_uint32
    let height = F.field t "height" int_uint32
    let mode_valid = F.field t "mode_valid" F.int
    let mode = F.field t "mode" DrmModeModeInfo.t
    let gamma_size = F.field t "gamma_size" F.int
    let () = F.seal t
  end

  module DrmModeSubPixel = struct
    type t =
      | Unknown
      | Horizontal_rgb
      | Horizontal_bgr
      | Vertical_rgb
      | Vertical_bgr
      | None

    let unknown = constant "DRM_MODE_SUBPIXEL_UNKNOWN" int64_t
    let horizontal_rgb = constant "DRM_MODE_SUBPIXEL_HORIZONTAL_RGB" int64_t
    let horizontal_bgr = constant "DRM_MODE_SUBPIXEL_HORIZONTAL_BGR" int64_t
    let vertical_rgb = constant "DRM_MODE_SUBPIXEL_VERTICAL_RGB" int64_t
    let vertical_bgr = constant "DRM_MODE_SUBPIXEL_VERTICAL_BGR" int64_t
    let none = constant "DRM_MODE_SUBPIXEL_NONE" int64_t

    let t =
      enum "drmModeSubPixel" ~typedef:true ~unexpected:(fun _ -> Unknown) [
        Unknown, unknown;
        Horizontal_rgb, horizontal_rgb;
        Horizontal_bgr, horizontal_bgr;
        Vertical_rgb, vertical_rgb;
        Vertical_bgr, vertical_bgr;
        None, none;
      ]
  end

  module DrmModeConnection = struct
    type t =
      | Connected
      | Disconnected
      | Unknown_connection

    let connected = constant "DRM_MODE_CONNECTED" int64_t
    let disconnected = constant "DRM_MODE_DISCONNECTED" int64_t
    let unknown_connection = constant "DRM_MODE_UNKNOWNCONNECTION" int64_t

    let t =
      enum "drmModeConnection" ~typedef:true ~unexpected:(fun _ -> Unknown_connection) [
        Connected, connected;
        Disconnected, disconnected;
        Unknown_connection, unknown_connection;
      ]
  end

  module DrmModeConnectorType = struct
    let v_Unknown = constant "DRM_MODE_CONNECTOR_Unknown" uint32_t
    let v_VGA = constant "DRM_MODE_CONNECTOR_VGA" uint32_t
    let v_DVII = constant "DRM_MODE_CONNECTOR_DVII" uint32_t
    let v_DVID = constant "DRM_MODE_CONNECTOR_DVID" uint32_t
    let v_DVIA = constant "DRM_MODE_CONNECTOR_DVIA" uint32_t
    let v_Composite = constant "DRM_MODE_CONNECTOR_Composite" uint32_t
    let v_SVIDEO = constant "DRM_MODE_CONNECTOR_SVIDEO" uint32_t
    let v_LVDS = constant "DRM_MODE_CONNECTOR_LVDS" uint32_t
    let v_Component = constant "DRM_MODE_CONNECTOR_Component" uint32_t
    let v_9PinDIN = constant "DRM_MODE_CONNECTOR_9PinDIN" uint32_t
    let v_DisplayPort = constant "DRM_MODE_CONNECTOR_DisplayPort" uint32_t
    let v_HDMIA = constant "DRM_MODE_CONNECTOR_HDMIA" uint32_t
    let v_HDMIB = constant "DRM_MODE_CONNECTOR_HDMIB" uint32_t
    let v_TV = constant "DRM_MODE_CONNECTOR_TV" uint32_t
    let v_eDP = constant "DRM_MODE_CONNECTOR_eDP" uint32_t
    let v_VIRTUAL = constant "DRM_MODE_CONNECTOR_VIRTUAL" uint32_t
    let v_DSI = constant "DRM_MODE_CONNECTOR_DSI" uint32_t
    let v_DPI = constant "DRM_MODE_CONNECTOR_DPI" uint32_t
    let v_WRITEBACK = constant "DRM_MODE_CONNECTOR_WRITEBACK" uint32_t
    let v_SPI = constant "DRM_MODE_CONNECTOR_SPI" uint32_t
    let v_USB = constant "DRM_MODE_CONNECTOR_USB" uint32_t
  end

  module DrmModeConnector = struct
    type mark
    type ctype = mark Ctypes.structure
    let t : ctype F.typ = F.structure "_drmModeConnector"

    let eid = encoder_id
    let connector_id = F.field t "connector_id" connector_id
    let encoder_id = F.field t "encoder_id" encoder_id_opt
    let connector_type = F.field t "connector_type" F.uint32_t
    let connector_type_id = F.field t "connector_type_id" int_uint32
    let connection = F.field t "connection" DrmModeConnection.t
    let mmWidth = F.field t "mmWidth" int_uint32
    let mmHeight = F.field t "mmHeight" int_uint32
    let subpixel = F.field t "subpixel" DrmModeSubPixel.t
    let count_modes = F.field t "count_modes" F.int
    let modes = F.field t "modes" (F.ptr DrmModeModeInfo.t)
    let count_props = F.field t "count_props" F.int
    let props = F.field t "props" (F.ptr property_id)
    let prop_values = F.field t "prop_values" (F.ptr F.uint64_t)
    let count_encoders = F.field t "count_encoders" F.int
    let encoders = F.field t "encoders" (F.ptr eid)
    let () = F.seal t
  end

  module DrmModeEncoderType = struct
    let v_None = constant "DRM_MODE_ENCODER_NONE" uint32_t
    let v_Dac = constant "DRM_MODE_ENCODER_DAC" uint32_t
    let v_Tmds = constant "DRM_MODE_ENCODER_TMDS" uint32_t
    let v_Lvds = constant "DRM_MODE_ENCODER_LVDS" uint32_t
    let v_Tvdac = constant "DRM_MODE_ENCODER_TVDAC" uint32_t
    let v_Virtual = constant "DRM_MODE_ENCODER_VIRTUAL" uint32_t
    let v_Dsi = constant "DRM_MODE_ENCODER_DSI" uint32_t
    let v_Dpmst = constant "DRM_MODE_ENCODER_DPMST" uint32_t
    let v_Dpi = constant "DRM_MODE_ENCODER_DPI" uint32_t
  end

  module DrmModeEncoder = struct
    type mark
    type ctype = mark Ctypes.structure
    let t : ctype F.typ = F.structure "_drmModeEncoder"

    let encoder_id = F.field t "encoder_id" encoder_id
    let encoder_type = F.field t "encoder_type" F.uint32_t
    let crtc_id = F.field t "crtc_id" crtc_id_opt
    let possible_crtcs = F.field t "possible_crtcs" F.uint32_t
    let possible_clones = F.field t "possible_clones" F.uint32_t
    let () = F.seal t
  end

  module DrmModePlaneRes = struct
    type mark
    type ctype = mark Ctypes.structure
    let t : ctype F.typ = F.structure "_drmModePlaneRes"

    let count_planes = F.field t "count_planes" int_uint32
    let planes = F.field t "planes" (F.ptr plane_id)
    let () = F.seal t
  end

  module DrmModePlane = struct
    type mark
    type ctype = mark Ctypes.structure
    let t : ctype F.typ = F.structure "_drmModePlane"

    let count_formats = F.field t "count_formats" int_uint32
    let formats = F.field t "formats" (F.ptr drm_format)
    let plane_id = F.field t "plane_id" plane_id
    let crtc_id = F.field t "crtc_id" crtc_id_opt
    let fb_id = F.field t "fb_id" fb_id_opt
    let crtc_x = F.field t "crtc_x" int_uint32
    let crtc_y = F.field t "crtc_y" int_uint32
    let x = F.field t "x" int_uint32
    let y = F.field t "y" int_uint32
    let possible_crtcs = F.field t "possible_crtcs" F.uint32_t
    let gamma_size = F.field t "gamma_size" F.uint32_t
    let () = F.seal t
  end

  module DrmModeObjectProperties = struct
    type mark
    type ctype = mark Ctypes.structure
    let t : ctype F.typ = F.structure "_drmModeObjectProperties"

    let count_props = F.field t "count_props" int_uint32
    let props = F.field t "props" (F.ptr property_id)
    let prop_values = F.field t "prop_values" (F.ptr F.uint64_t)
    let () = F.seal t
  end

  module DrmModeObjectType = struct
    let crtc = constant "DRM_MODE_OBJECT_CRTC" uint32_t
    let connector = constant "DRM_MODE_OBJECT_CONNECTOR" uint32_t
    let encoder = constant "DRM_MODE_OBJECT_ENCODER" uint32_t
    let mode = constant "DRM_MODE_OBJECT_MODE" uint32_t
    let property = constant "DRM_MODE_OBJECT_PROPERTY" uint32_t
    let fb = constant "DRM_MODE_OBJECT_FB" uint32_t
    let blob = constant "DRM_MODE_OBJECT_BLOB" uint32_t
    let plane = constant "DRM_MODE_OBJECT_PLANE" uint32_t
    let any = constant "DRM_MODE_OBJECT_ANY" uint32_t
  end

  module DrmModePropertyEnum = struct
    type mark
    type ctype = mark Ctypes.structure
    let t : ctype F.typ = F.structure "drm_mode_property_enum"

    let value = F.field t "value" uint64_t
    let name = F.field t "name" (F.array Config.drm_prop_name_len F.char)
    let () = F.seal t
  end

  module DrmModeProperty = struct
    module Flags = struct
      let pending = constant "DRM_MODE_PROP_PENDING" uint32_t
      let range = constant "DRM_MODE_PROP_RANGE" uint32_t
      let immutable = constant "DRM_MODE_PROP_IMMUTABLE" uint32_t
      let enum = constant "DRM_MODE_PROP_ENUM" uint32_t
      let blob = constant "DRM_MODE_PROP_BLOB" uint32_t
      let bitmask = constant "DRM_MODE_PROP_BITMASK" uint32_t
      let object_ = constant "DRM_MODE_PROP_OBJECT" uint32_t
      let signed_range = constant "DRM_MODE_PROP_SIGNED_RANGE" uint32_t
    end

    type mark
    type ctype = mark Ctypes.structure
    let t : ctype F.typ = F.structure "_drmModeProperty"

    let prop_id = F.field t "prop_id" property_id
    let flags = F.field t "flags" flags32
    let name = F.field t "name" (F.array Config.drm_prop_name_len F.char)
    let count_values = F.field t "count_values" F.int
    let values = F.field t "values" (F.ptr F.uint64_t)
    let count_enums = F.field t "count_enums" F.int
    let enums = F.field t "enums" (ptr DrmModePropertyEnum.t)
    let count_blobs = F.field t "count_blobs" F.int
    let blob_ids = F.field t "blob_ids" (F.ptr blob_id)
    let () = F.seal t
  end

  module DrmModePropertyBlob = struct
    type mark
    type ctype = mark Ctypes.structure
    let t : ctype F.typ = F.structure "_drmModePropertyBlob"

    let id = F.field t "id" blob_id
    let length = F.field t "length" int_uint32
    let data = F.field t "data" (F.ptr F.void)
    let () = F.seal t
  end

  module Cap = struct
    type 'a ty = Int : int ty | Bool : bool ty
    type 'a t = 'a ty * Unsigned.UInt64.t

    let bool x = Bool, constant x uint64_t
    let int x = Int, constant x uint64_t

    let dumb_buffer = bool "DRM_CAP_DUMB_BUFFER"
    let vblank_high_crtc = bool "DRM_CAP_VBLANK_HIGH_CRTC"
    let dumb_preferred_depth = int "DRM_CAP_DUMB_PREFERRED_DEPTH"
    let dumb_prefer_shadow = bool "DRM_CAP_DUMB_PREFER_SHADOW"
    let prime = int "DRM_CAP_PRIME"
    let timestamp_monotonic = bool "DRM_CAP_TIMESTAMP_MONOTONIC"
    let async_page_flip = Bool, Unsigned.UInt64.of_int 0x7  (* bool "DRM_CAP_ASYNC_PAGE_FLIP" -- Debian 12 too old *)
    let cursor_width = int "DRM_CAP_CURSOR_WIDTH"
    let cursor_height = int "DRM_CAP_CURSOR_HEIGHT"
    let addfb2_modifiers = bool "DRM_CAP_ADDFB2_MODIFIERS"
    let page_flip_target = bool "DRM_CAP_PAGE_FLIP_TARGET"
    let crtc_in_vblank_event = bool "DRM_CAP_CRTC_IN_VBLANK_EVENT"
    let syncobj = bool "DRM_CAP_SYNCOBJ"
    let syncobj_timeline = bool "DRM_CAP_SYNCOBJ_TIMELINE"
    let atomic_async_page_flip = Bool, Unsigned.UInt64.of_int 0x15  (* bool "DRM_CAP_ATOMIC_ASYNC_PAGE_FLIP" *)
  end

  module Client_cap = struct
    type t = Unsigned.UInt64.t
    let t = Ctypes.uint64_t

    let stereo_3d = constant "DRM_CLIENT_CAP_STEREO_3D" F.uint64_t
    let universal_planes = constant "DRM_CLIENT_CAP_UNIVERSAL_PLANES" F.uint64_t
    let atomic = constant "DRM_CLIENT_CAP_ATOMIC" F.uint64_t
    let aspect_ratio = constant "DRM_CLIENT_CAP_ASPECT_RATIO" F.uint64_t
    let writeback_connectors = constant "DRM_CLIENT_CAP_WRITEBACK_CONNECTORS" F.uint64_t

    (* Debian 12 didn't have this, so just hard-code it for now *)
    let cursor_plane_hotspot = Unsigned.UInt64.of_int 6
    (* let cursor_plane_hotspot = constant "DRM_CLIENT_CAP_CURSOR_PLANE_HOTSPOT" F.uint64_t *)
  end

  module DrmModeFB2 = struct
    type mark
    type ctype = mark Ctypes.structure
    let t : ctype F.typ = F.structure "_drmModeFB2"

    let fb_id = F.field t "fb_id" fb_id
    let width = F.field t "width" int_uint32
    let height = F.field t "height" int_uint32
    let pixel_format = F.field t "pixel_format" drm_format
    let modifier = F.field t "modifier" drm_modifier
    let flags = F.field t "flags" flags32
    let handles = F.field t "handles" (F.array 4 buffer_id_opt)
    let pitches = F.field t "pitches" (F.array 4 pitch)
    let offsets = F.field t "offsets" (F.array 4 offset)
    let () = F.seal t
  end

  module AtomicReqPtr : sig
    type t
    val t : t ptr typ
    val t_opt : t ptr option typ
  end = struct
    type t = unit
    let t = typedef (ptr void) "drmModeAtomicReqPtr"
    let t_opt = typedef (ptr_opt void) "drmModeAtomicReqPtr"
  end

  module AtomicFlags = struct
    let test_only = constant "DRM_MODE_ATOMIC_TEST_ONLY" uint32_t
    let nonblock = constant "DRM_MODE_ATOMIC_NONBLOCK" uint32_t
    let allow_modeset = constant "DRM_MODE_ATOMIC_ALLOW_MODESET" uint32_t
  end

  module Dma_buf_export_sync_file = struct
    type mark
    type ctype = mark Ctypes.structure
    let t : ctype F.typ = F.structure "dma_buf_export_sync_file"

    let flags = F.field t "flags" uint32_t
    let fd = F.field t "fd" fd
    let () = F.seal t
  end

  module Dma_buf_import_sync_file = struct
    type mark
    type ctype = mark Ctypes.structure
    let t : ctype F.typ = F.structure "dma_buf_import_sync_file"

    let flags = F.field t "flags" uint32_t
    let fd = F.field t "fd" fd
    let () = F.seal t
  end
end
