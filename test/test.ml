module Kms = Drm.Kms
module U64 = Unsigned.UInt64

let println fmt = Fmt.pr (fmt ^^ "@.")

let open_device (d : Drm.Device.Info.t) =
  match d.primary_node with
  | None -> None
  | Some primary ->
    let fd = Unix.openfile primary [O_RDWR; O_CLOEXEC] 0 in
    if not (Drm.Device.is_kms fd) then None
    else (
      println "Version: %a" Drm.Device.Version.pp (Drm.Device.Version.get fd);
      Some fd
    )

let pp_connector fd f (x : Kms.Connector.t) =
  if x.connection = Connected then (
    let props = Kms.Properties.of_raw fd x.connector_id x.props in
    Fmt.pf f "%a@,%a"
      Kms.Connector.pp x
      Kms.Properties.pp props
  ) else Fmt.pf f "%a (%a)" Kms.Connector.pp_name x Kms.Connector.Connection.pp x.connection

let pp_encoder f (x : Kms.Encoder.t) =
  Fmt.pf f "@[<h>%d (%a) {crtc_id = %a;@ possible_crtcs = %#x;@ possible_clones = %#x}@]"
    (x.encoder_id :> int) Kms.Encoder.Type.pp x.encoder_type (Fmt.Dump.option Drm.Id.pp) x.crtc_id x.possible_crtcs x.possible_clones

let pp_format f (fmt, modifier) =
  Fmt.pf f "%a:%a"
    Drm.Fourcc.pp fmt
    Drm.Modifier.pp modifier

let pp_plane fd f (x : Kms.Plane.t) =
  if x.crtc_id = None then Fmt.pf f "%d (unused)" (x.plane_id :> int)
  else (
    let props = Kms.Plane.get_properties fd x.plane_id in
    let in_formats = Kms.Properties.get_value props (Kms.Plane.in_formats fd) in
    Fmt.pf f "%a@,%a@,IN_FORMATS: %a"
      Kms.Plane.pp x
      Kms.Properties.pp props
      (Fmt.Dump.option (Fmt.Dump.list pp_format)) in_formats;
  )

let pp_crtc f (x : Kms.Crtc.t) =
  match x.mode with
  | None -> Fmt.pf f "%d (inactive)" (x.crtc_id :> int)
  | Some mode ->
    Fmt.pf f "%a@,Mode: %a" Kms.Crtc.pp x Kms.Mode_info.pp mode

let test_sync_file dmabuf_fd =
  let sync_fd = Drm.Dmabuf.export_sync_file dmabuf_fd `RW in
  Drm.Dmabuf.import_sync_file dmabuf_fd ~sync_file_fd:sync_fd `RW;
  Unix.close sync_fd

let () =
  match Drm.get_devices ~get_pci_revision:true () with
  | exception Unix.Unix_error (ENOENT, _, _) ->
    println "DRM not supported on this platform; skipping tests"
  | devices ->
    println "@[<v2>devices:@,%a@]" (Fmt.Dump.list Drm.Device.Info.pp) devices;
    match List.find_map open_device devices with
    | None -> println "No suitable device found; skipping tests"
    | Some dev ->
      begin
        match Drm.Client_cap.(set atomic) dev true with
        | Ok () -> ()
        | Error code -> println "Atomic mode-setting not supported: %s" (Unix.error_message code)
      end;
      Drm.Client_cap.(set_exn universal_planes) dev true;
      let mode_res = Kms.Resources.get dev in
      println "Resources: %a" Kms.Resources.pp mode_res;
      let encoders = List.map (Kms.Encoder.get dev) mode_res.encoders in
      println "@[<v2>Encoders:@,%a@]" (Fmt.Dump.list pp_encoder) encoders;
      let connectors = List.map (Kms.Connector.get dev) mode_res.connectors in
      println "@[<v2>Connectors:@,%a@]" (Fmt.Dump.list (pp_connector dev)) connectors;
      let planes = Kms.Plane_resources.get dev |> List.map (Kms.Plane.get dev) in
      println "@[<v2>Planes:@,%a@]" (Fmt.Dump.list (pp_plane dev)) planes;
      let crtcs = List.map (Kms.Crtc.get dev) mode_res.crtcs in
      println "@[<v2>CRTCs:@,%a@]" (Fmt.Dump.list pp_crtc) crtcs;
      let cw = Drm.Cap.(get_exn cursor_width) dev in
      let ch = Drm.Cap.(get_exn cursor_height) dev in
      println "Suggested cursor size: %dx%d" cw ch;

      let dumb_buffer = Drm.Buffer.Dumb.create dev ~bpp:32 ~size:(640, 480) in
      println "Dumb buffer handle = %d" (dumb_buffer.handle :> int);
      let plane = { Kms.Fb.Plane.handle = dumb_buffer.handle; pitch = dumb_buffer.pitch; offset = 0 } in
      let fb_id = Kms.Fb.add dev ~size:(640, 480) ~planes:[plane] ~pixel_format:Drm.Fourcc.xr24 in
      let fb = Kms.Fb.get dev fb_id in
      println "Framebuffer: %a" Kms.Fb.pp fb;
      Kms.Fb.close_plane_handles dev fb;
      let prime_fd = Drm.Dmabuf.of_handle ~rw:false dev dumb_buffer.handle in
      test_sync_file prime_fd;
      let imported_handle = Drm.Dmabuf.to_handle dev prime_fd in
      assert (imported_handle == dumb_buffer.handle);
      Unix.close prime_fd;
      Kms.Fb.rm dev fb_id;
      Drm.Buffer.close dev dumb_buffer.handle
