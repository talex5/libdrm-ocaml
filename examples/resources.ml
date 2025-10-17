module Kms = Drm.Kms

type t = {
  dev : Drm.Device.t;
  crtcs : Kms.Crtc.t list;
  connectors : Kms.Connector.t list;
  encoders : Kms.Encoder.t list;
  planes : Kms.Plane.t list;
}

let get dev =
  let mode_res = Kms.Resources.get dev in
  let encoders = List.map (Kms.Encoder.get dev) mode_res.encoders in
  let connectors = List.map (Kms.Connector.get dev) mode_res.connectors in
  let crtcs = List.map (Kms.Crtc.get dev) mode_res.crtcs in
  let planes = List.map (Kms.Plane.get dev) (Kms.Plane_resources.get dev) in
  { dev; crtcs; encoders; connectors; planes }

let crtc_index t crtc_id =
  List.find_index (fun (crtc : Kms.Crtc.t) -> crtc.crtc_id = crtc_id) t.crtcs
  |> Option.get
