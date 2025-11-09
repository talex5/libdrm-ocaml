(** Graphics devices. *)

type t = Unix.file_descr
(** Typically a device inside the [/dev/dri/] directory.
    
    Use {!list} to find devices. *)

(** {2 Finding devices} *)

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

end

val list : ?get_pci_revision:bool -> unit -> Info.t list
(** [list ()] returns available graphics devices.

    @param get_pci_revision If [false], skip getting the PCI device revision field.
                            The default is [true]. *)

(** {2 Querying devices} *)

module Version : sig
  (** Metadata about a device driver. *)

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

val is_kms : t -> bool
(** Whether this device supports the {!Kms} API. *)

val check_modesetting_supported : string -> (unit, Unix.error) result
(** [check_modesetting_supported busid] checks if a modesetting capable driver has attached to the PCI ID [busid].

    Returns {!Unix.EINVAL} for invalid bus id and {!Unix.ENOSYS} if no modesetting support. *)

(** {2 DRM masters} *)

val is_master : t -> bool
(** Whether this device is currently the DRM master. *)

val drop_master : t -> unit
(** Give up being DRM master for now.

    This allows the user to switch away to another VT. *)

val set_master : t -> unit
(** Become the DRM master.

    This can only be called if [t] has been the master in the past or you have CAP_SYS_ADMIN,
    and if there is no current master.
    Opening a DRM device without a master makes you the master.
    This function is useful to become the master again after a VT switch. *)
