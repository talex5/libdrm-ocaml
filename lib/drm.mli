(** OCaml bindings for {{: https://gitlab.freedesktop.org/mesa/libdrm } libdrm}.
    This library can be used to find GPU devices, configure monitors, etc. *)

module Device = Device

val get_devices : ?get_pci_revision:bool -> unit -> Device.Info.t list
(** Enumerate all devices available on the system. *)

module Cap = Cap
module Client_cap = Client_cap
module Id = Id
module Buffer = Buffer
module Kms = Kms
module Fourcc = Fourcc
module Modifier = Modifier
module Dev_t = Dev_t
module Dmabuf = Dmabuf
