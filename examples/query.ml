module K = Drm.Kms
module U64 = Unsigned.UInt64

let println fmt = Fmt.pr (fmt ^^ "@.")

let pp_connector fd f (x : K.Connector.t) =
  if x.connection = Connected then (
    let props = K.Properties.of_raw fd x.connector_id x.props in
    Fmt.pf f "%a@,%a"
      K.Connector.pp x
      K.Properties.pp props
  ) else Fmt.pf f "%a (%a)" K.Connector.pp_name x K.Connector.Connection.pp x.connection

let pp_encoder f (x : K.Encoder.t) =
  Fmt.pf f "@[<h>%d (%a) {crtc_id = %a;@ possible_crtcs = %#x;@ possible_clones = %#x}@]"
    (x.encoder_id :> int) K.Encoder.Type.pp x.encoder_type (Fmt.Dump.option Drm.Id.pp) x.crtc_id x.possible_crtcs x.possible_clones

let pp_format f (fmt, modifier) =
  Fmt.pf f "%a:%a"
    Drm.Fourcc.pp fmt
    Drm.Modifier.pp modifier

let pp_plane fd f (x : K.Plane.t) =
  if x.crtc_id = None then Fmt.pf f "%d (unused)" (x.plane_id :> int)
  else (
    let props = K.Plane.get_properties fd x.plane_id in
    let in_formats = K.Properties.get_value props (K.Plane.in_formats fd) in
    Fmt.pf f "%a@,%a@,IN_FORMATS: %a"
      K.Plane.pp x
      K.Properties.pp props
      (Fmt.Dump.option (Fmt.Dump.list pp_format)) in_formats;
  )

let pp_crtc f (x : K.Crtc.t) =
  match x.mode with
  | None -> Fmt.pf f "%d (inactive)" (x.crtc_id :> int)
  | Some mode ->
    Fmt.pf f "%a@,Mode: %a" K.Crtc.pp x K.Mode_info.pp mode

let test_sync_file dmabuf_fd =
  let sync_fd = Drm.Dmabuf.export_sync_file dmabuf_fd `RW in
  Drm.Dmabuf.import_sync_file dmabuf_fd ~sync_file_fd:sync_fd `RW;
  Unix.close sync_fd

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
      let planes = K.Plane_resources.get dev |> List.map (K.Plane.get dev) in
      println "@[<v2>Planes:@,%a@]" (Fmt.Dump.list (pp_plane dev)) planes;
      let crtcs = List.map (K.Crtc.get dev) mode_res.crtcs in
      println "@[<v2>CRTCs:@,%a@]" (Fmt.Dump.list pp_crtc) crtcs;
      let cw = Drm.Cap.(get_exn cursor_width) dev in
      let ch = Drm.Cap.(get_exn cursor_height) dev in
      println "Suggested cursor size: %dx%d" cw ch;

      let dumb_buffer = Drm.Buffer.Dumb.create dev ~bpp:32 (640, 480) in
      println "Dumb buffer handle = %a" Drm.Id.pp dumb_buffer.handle;
      let plane = K.Fb.Plane.v dumb_buffer.handle ~pitch:dumb_buffer.pitch in
      let fb_id = K.Fb.add dev ~size:(640, 480) ~planes:[plane] ~pixel_format:Drm.Fourcc.xr24 in
      let fb = K.Fb.get dev fb_id in
      println "Framebuffer: %a" K.Fb.pp fb;
      K.Fb.close_plane_handles dev fb;
      let prime_fd = Drm.Dmabuf.of_handle ~rw:false dev dumb_buffer.handle in
      test_sync_file prime_fd;
      let imported_handle = Drm.Dmabuf.to_handle dev prime_fd in
      assert (imported_handle = dumb_buffer.handle);
      Unix.close prime_fd;
      K.Fb.rm dev fb_id;
      Drm.Buffer.close dev dumb_buffer.handle
