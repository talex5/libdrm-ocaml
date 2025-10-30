module K = Drm.Kms

let println fmt = Fmt.pr (fmt ^^ "@.")

let show_test_page (t : Resources.t) (c : K.Connector.t) =
  match c.encoder_id with
  | None -> println "%a has no encoder (skipping)" K.Connector.pp_name c
  | Some encoder_id ->
    match K.Encoder.get t.dev encoder_id with
    | { crtc_id = None; _ } ->
      println "%a's encoder has no CRTC (skipping)" K.Connector.pp_name c
    | { crtc_id = Some crtc_id; _ } ->
      println "Showing test page on %a" K.Connector.pp_name c;
      let mode = List.hd c.modes in
      let size = (mode.hdisplay, mode.vdisplay) in
      let fb = Test_image.create t.dev size in
      K.Crtc.set t.dev crtc_id (Some mode) ~fb ~pos:(0,0)
        ~connectors:[c.connector_id]

let () =
  Utils.with_device @@ fun t ->
  let connected = List.filter Utils.is_connected t.connectors in
  Utils.restore_modes_on_exit t @@ fun () ->
  List.iter (show_test_page t) connected;
  Unix.sleep 2
