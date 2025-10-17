module CT = C.Types

type t = Unix.file_descr

let ( !@ ) = Ctypes.( !@ )

module Info = struct
  module PciBus = struct
    open CT.DrmPciBusInfo

    type t = {
      domain : int;
      bus : int;
      dev : int;
      func : int;
    }
    let of_c c = {
      domain = Ctypes.getf c domain;
      bus = Ctypes.getf c bus;
      dev = Ctypes.getf c dev;
      func = Ctypes.getf c func;
    }
    let pp f t =
      Fmt.pf f "{@[<hv>domain = %d;@ bus = %d;@ dev = %d;@ func = %d@]}"
        t.domain t.bus t.dev t.func
  end

  module PciDevice = struct
    open CT.DrmPciDeviceInfo

    type t = {
      vendor_id : int;
      device_id : int;
      subvendor_id : int;
      subdevice_id : int;
      revision_id : int;
    }
    let of_c c = {
      vendor_id = Ctypes.getf c vendor_id;
      device_id = Ctypes.getf c device_id;
      subvendor_id = Ctypes.getf c subvendor_id;
      subdevice_id = Ctypes.getf c subdevice_id;
      revision_id = Ctypes.getf c revision_id;
    }
    let pp f t =
      Fmt.pf f "{@[<hv>vendor_id = %#x;@ device_id = %#x;@ subvendor_id = %#x;@ subdevice_id = %#x;@ revision_id = %#x@]}"
        t.vendor_id t.device_id t.subvendor_id t.subdevice_id t.revision_id
  end

  module UsbBus = struct
    open CT.DrmUsbBusInfo

    type t = {
      bus : int;
      dev : int;
    }
    let of_c c = {
      bus = Ctypes.getf c bus;
      dev = Ctypes.getf c dev;
    }
    let pp f t =
      Fmt.pf f "{@[<hv>bus = %d;@ dev = %d;@]}"
        t.bus t.dev
  end

  module UsbDevice = struct
    open CT.DrmUsbDeviceInfo

    type t = {
      vendor : int;
      product : int;
    }
    let of_c c = {
      vendor = Ctypes.getf c vendor;
      product = Ctypes.getf c product;
    }
    let pp f t =
      Fmt.pf f "{@[<hv>vendor = %d;@ product = %d;@]}"
        t.vendor t.product
  end

  module PlatformBus = struct
    open CT.DrmPlatformBusInfo

    type t = {
      fullname : string;
    }
    let of_c c = {
      fullname = Ctypes.(coerce (ptr char) string) (Ctypes.getf c fullname).astart;
    }
    let pp f t =
      Fmt.pf f "{@[<hv>fullname = %S@]}"
        t.fullname
  end

  module PlatformDevice = struct
    open CT.DrmPlatformDeviceInfo
    open Ctypes

    type t = {
      compatible : string list;
    }
    let of_c c =
      let rec collect ptr =
        if Ctypes.is_null ptr then []
        else (
          let v = Ctypes.(coerce (ptr char) string) (!@ ptr) in
          v :: collect (ptr +@ 1)
        )
      in
      { compatible = List.rev (collect (Ctypes.getf c compatible)) }

    let pp f t =
      Fmt.pf f "{@[<hv>compatible = %a@]}"
        Fmt.(Dump.list string)
        t.compatible
  end

  module Host1xBus = struct
    open CT.DrmHost1xBusInfo

    type t = {
      fullname : string;
    }
    let of_c c = {
      fullname = Ctypes.(coerce (ptr char) string) (Ctypes.getf c fullname).astart;
    }
    let pp f t =
      Fmt.pf f "{@[<hv>fullname = %S@]}"
        t.fullname
  end

  module Host1xDevice = PlatformDevice

  let deref x = !@ x

  type info =
    | PCI of PciBus.t * PciDevice.t
    | USB of UsbBus.t * UsbDevice.t
    | PLATFORM of PlatformBus.t * PlatformDevice.t
    | HOST1X of Host1xBus.t * Host1xDevice.t
    | Unknown of int

  type t = {
    primary_node : string option;
    render_node : string option;
    info : info;
  }

  let pp_info f = function
    | PCI (bus, dev) ->
      Fmt.pf f "@[<v2>PCI @[<hv1>{bus = %a;@ dev = %a}@]@]"
        PciBus.pp bus
        PciDevice.pp dev
    | USB (bus, dev) ->
      Fmt.pf f "@[<v2>USB @[<hv1>{bus = %a;@ dev = %a}@]@]"
        UsbBus.pp bus
        UsbDevice.pp dev
    | PLATFORM (bus, dev) ->
      Fmt.pf f "@[<v2>PLATFORM @[<hv1>{bus = %a;@ dev = %a}@]@]"
        PlatformBus.pp bus
        PlatformDevice.pp dev
    | HOST1X (bus, dev) ->
      Fmt.pf f "@[<v2>HOST1X @[<hv1>{bus = %a;@ dev = %a}@]@]"
        Host1xBus.pp bus
        Host1xDevice.pp dev
    | Unknown x -> Fmt.pf f "Unknown %d" x

  let pp f { primary_node; render_node; info } =
    let node = Fmt.Dump.option (Fmt.fmt "%S") in
    Fmt.pf f "{@[<hv>primary_node = %a;@ render_node = %a;@ info = %a@]}"
      node primary_node
      node render_node
      pp_info info

  let of_c c =
    let module T = C.Types.Device in
    let available_nodes = Ctypes.getf c T.available_nodes in
    let nodes = !@ (Ctypes.getf c T.nodes) in
    let get i =
      if available_nodes land (1 lsl i) = 0 then None
      else (
        let node =
          Ctypes.CArray.get nodes i
          |> Ctypes.(coerce (ptr char) string)
        in
        Some node
      )
    in
    let primary_node = get C.Types.Node.primary in
    let render_node = get C.Types.Node.render in
    let businfo = Ctypes.getf c T.businfo in
    let devinfo = Ctypes.getf c T.deviceinfo in
    let info =
      match Ctypes.getf c T.bustype with
      | 0 ->
        let bus = Ctypes.getf businfo C.Types.Businfo.pci |> deref |> PciBus.of_c in
        let dev = Ctypes.getf devinfo C.Types.Deviceinfo.pci |> deref |> PciDevice.of_c in
        PCI (bus, dev)
      | 1 ->
        let bus = Ctypes.getf businfo C.Types.Businfo.usb |> deref |> UsbBus.of_c in
        let dev = Ctypes.getf devinfo C.Types.Deviceinfo.usb |> deref |> UsbDevice.of_c in
        USB (bus, dev)
      | 2 ->
        let bus = Ctypes.getf businfo C.Types.Businfo.platform |> deref |> PlatformBus.of_c in
        let dev = Ctypes.getf devinfo C.Types.Deviceinfo.platform |> deref |> PlatformDevice.of_c in
        PLATFORM (bus, dev)
      | 3 ->
        let bus = Ctypes.getf businfo C.Types.Businfo.host1x |> deref |> Host1xBus.of_c in
        let dev = Ctypes.getf devinfo C.Types.Deviceinfo.host1x |> deref |> Host1xDevice.of_c in
        HOST1X (bus, dev)
      | x -> Unknown x
    in
    { primary_node; render_node; info }

  let get_devices ?(get_pci_revision=false) () =
    let flags = if get_pci_revision then C.Types.Deviceinfo.get_pci_revision else Unsigned.UInt32.of_int 0 in
    let n_devices, errno = C.Functions.drmGetDevices2 flags None 0 in
    if n_devices < 0 then Err.report errno "drmGetDevices2" "";
    let devices = Ctypes.CArray.make (Ctypes.ptr C.Types.Device.t) n_devices in
    let r, errno = C.Functions.drmGetDevices2 flags (Some devices.astart) n_devices in
    if r < 0 then Err.report errno "drmGetDevices2" "";
    assert (r = n_devices);
    let results = Ctypes.CArray.to_list devices |> List.map (fun ptr -> of_c (!@ ptr)) in
    C.Functions.drmFreeDevices devices.astart n_devices |> Err.ignore;
    results
end

module Version = struct
  open CT.DrmVersion

  type t = {
    version_major : int;
    version_minor : int;
    version_patchlevel : int;
    name : string;
    date : string;
    desc : string;
  }

  let of_c c =
    let str len_field ptr_field =
      let length = Ctypes.getf c len_field in
      let ptr = Ctypes.getf c ptr_field in
      Ctypes.string_from_ptr ptr ~length
    in
    {
      version_major = Ctypes.getf c version_major;
      version_minor = Ctypes.getf c version_minor;
      version_patchlevel = Ctypes.getf c version_patchlevel;
      name = str name_len name;
      date = str date_len date;
      desc = str desc_len desc;
    }
  let pp f t =
    Fmt.pf f "{@[<hv>version = %d.%d.%d;@ name = %S;@ date = %S;@ desc = %S@]}"
      t.version_major t.version_minor t.version_patchlevel
      t.name t.date t.desc

  let get fd =
    match C.Functions.drmGetVersion fd with
    | None, errno -> Err.report errno "drmGetVersion" ""
    | Some c, _ ->
      let x = of_c (!@ c) in
      C.Functions.drmFreeVersion c |> Err.ignore;
      x
end

let is_kms t = C.Functions.drmIsKMS t |> Err.ignore
let is_master t = C.Functions.drmIsMaster t |> Err.ignore
