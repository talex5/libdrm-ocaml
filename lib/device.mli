(** Graphics devices. *)

type t = Unix.file_descr
(** Typically a device inside the [/dev/dri/] directory.
    
    Use {!Drm.get_devices} to find devices. *)

val is_kms : t -> bool
(** Whether this device supports the {!Kms} API. *)

val is_master : t -> bool
(** Whether this device is currently the DRM master. *)

module Version : sig
  (** Metadata about the device driver. *)

  type device := t

  type t = {
    version_major : int;
    version_minor : int;
    version_patchlevel : int;
    name : string;
    date : string;
    desc : string;
  }

  val get : device -> t

  val pp : t Fmt.t [@@ocaml.toplevel_printer]
end

module Info : sig
  module PciBus : sig
    type t = { domain : int; bus : int; dev : int; func : int }
    val pp : t Fmt.t
  end

  module PciDevice : sig
    type t = {
      vendor_id : int;
      device_id : int;
      subvendor_id : int;
      subdevice_id : int;
      revision_id : int;
    }
    val pp : t Fmt.t
  end

  module UsbBus : sig
    type t = { bus : int; dev : int }
    val pp : t Fmt.t
  end

  module UsbDevice : sig
    type t = { vendor : int; product : int }
    val pp : t Fmt.t
  end

  module PlatformBus : sig
    type t = { fullname : string }
    val pp : t Fmt.t
  end

  module PlatformDevice : sig
    type t = { compatible : string list }
    val pp : t Fmt.t
  end

  module Host1xBus : sig
    type t = { fullname : string }
    val pp : t Fmt.t
  end

  module Host1xDevice : sig
    type t = { compatible : string list }
    val pp : t Fmt.t
  end

  type info =
    | PCI of PciBus.t * PciDevice.t
    | USB of UsbBus.t * UsbDevice.t
    | PLATFORM of PlatformBus.t * PlatformDevice.t
    | HOST1X of Host1xBus.t * Host1xDevice.t
    | Unknown of int

  type t = {
    primary_node : string option;       (** Gives full access to the device, including KMS. *)
    render_node : string option;        (** Rendering access only. *)
    info : info;
  }

  val pp : t Fmt.t [@@ocaml.toplevel_printer]

  (**/**)
  val get_devices : ?get_pci_revision:bool -> unit -> t list
end
