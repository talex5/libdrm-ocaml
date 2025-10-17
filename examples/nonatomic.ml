module Kms = Drm.Kms

let println fmt = Fmt.pr (fmt ^^ "@.")

let show_test_page (t : Resources.t) (c : Kms.Connector.t) =
  match c.encoder_id with
  | None -> println "Connector %a has no encoder (skipping)" Kms.Connector.pp_name c
  | Some encoder_id ->
    match Kms.Encoder.get t.dev encoder_id with
    | { crtc_id = None; _ } ->
      println "Connector %a's encoder has no CRTC (skipping)" Kms.Connector.pp_name c
    | { crtc_id = Some crtc_id; _ } ->
      println "Showing test page on %a" Kms.Connector.pp_name c;
      let mode = List.hd c.modes in
      let size = (mode.hdisplay, mode.vdisplay) in
      let dumb_buffer = Test_image.create t.dev size in
      let plane = { Kms.Fb.Plane.handle = dumb_buffer.handle; pitch = dumb_buffer.pitch; offset = 0 } in
      let fb = Kms.Fb.add t.dev ~size ~planes:[plane] ~pixel_format:Drm.Fourcc.xr24 in
      Kms.Crtc.set t.dev crtc_id (Some mode) ~buffer:fb ~pos:(0,0) ~connectors:[c.connector_id]

let () =
  Utils.with_device @@ fun t ->
  let connected = List.filter Utils.is_connected t.connectors in
  Utils.restore_modes_on_exit t @@ fun () ->
  List.iter (show_test_page t) connected;
  Unix.sleep 2
