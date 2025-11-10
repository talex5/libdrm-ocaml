module K = Drm.Kms
module U64 = Unsigned.UInt64

let println fmt = Fmt.pr (fmt ^^ "@.")

let pp_connector dev f (x : K.Connector.t) =
  if x.connection = Connected then (
    let props = K.Properties.Values.of_raw dev Connector x.connector_id x.props in
    Fmt.pf f "%a@,%a"
      K.Connector.pp x
      K.Properties.Values.pp props
  ) else Fmt.pf f "%a (%a)" K.Connector.pp_name x K.Connector.Connection.pp x.connection

let pp_encoder f (x : K.Encoder.t) =
  Fmt.pf f "@[<h>%d (%a) {crtc_id = %a;@ possible_crtcs = %#x;@ possible_clones = %#x}@]"
    (x.encoder_id :> int) K.Encoder.Type.pp x.encoder_type (Fmt.Dump.option Drm.Id.pp) x.crtc_id x.possible_crtcs x.possible_clones

let pp_format f (fmt, modifier) =
  Fmt.pf f "%a:%a"
    Drm.Fourcc.pp fmt
    Drm.Modifier.pp modifier

let pp_plane dev f (x : K.Plane.t) =
  if x.crtc_id = None then Fmt.pf f "%d (unused)" (x.plane_id :> int)
  else (
    let props = K.Plane.get_properties dev x.plane_id in
    let in_formats_id = K.Properties.Values.get_value props K.Plane.in_formats in
    let in_formats = Option.map (K.Plane.get_in_formats dev) in_formats_id in
    Fmt.pf f "%a@,%a@,IN_FORMATS: %a"
      K.Plane.pp x
      K.Properties.Values.pp props
      (Fmt.Dump.option (Fmt.Dump.list pp_format)) in_formats;
  )

let pp_crtc dev f (x : K.Crtc.t) =
  match x.mode with
  | None -> Fmt.pf f "%d (inactive)" (x.crtc_id :> int)
  | Some mode ->
    let props = K.Crtc.get_properties dev x.crtc_id in
    Fmt.pf f "%a@,Mode: %a@,Props: %a"
      K.Crtc.pp x
      K.Mode_info.pp mode
      K.Properties.Values.pp props

let () =
  match Drm.Device.list ~get_pci_revision:true () with
  | exception Unix.Unix_error (ENOENT, _, _) ->
    println "DRM not supported on this platform; skipping tests"
  | devices ->
    println "@[<v2>devices:@,%a@]" (Fmt.Dump.list Drm.Device.Info.pp) devices;
    match List.find_map Utils.open_device devices with
    | None -> println "No suitable device found; skipping tests"
    | Some dev ->
      println "Version: %a" Drm.Device.Version.pp (Drm.Device.Version.get dev);
      begin
        match Drm.Client_cap.(set atomic) dev true with
        | Ok () -> ()
        | Error code -> println "Atomic mode-setting not supported: %s" (Unix.error_message code)
      end;
      Drm.Client_cap.(set_exn universal_planes) dev true;
      let mode_res = K.Resources.get dev in
      println "Resources: %a" K.Resources.pp mode_res;
      let encoders = List.map (K.Encoder.get dev) mode_res.encoders in
      println "@[<v2>Encoders:@,%a@]" (Fmt.Dump.list pp_encoder) encoders;
      let connectors = List.map (K.Connector.get dev) mode_res.connectors in
      println "@[<v2>Connectors:@,%a@]" (Fmt.Dump.list (pp_connector dev)) connectors;
      let planes = K.Plane.list dev |> List.map (K.Plane.get dev) in
      println "@[<v2>Planes:@,%a@]" (Fmt.Dump.list (pp_plane dev)) planes;
      let crtcs = List.map (K.Crtc.get dev) mode_res.crtcs in
      println "@[<v2>CRTCs:@,%a@]" (Fmt.Dump.list (pp_crtc dev)) crtcs;
      let cw = Drm.Cap.(get_exn cursor_width) dev in
      let ch = Drm.Cap.(get_exn cursor_height) dev in
      println "Suggested cursor size: %dx%d" cw ch
