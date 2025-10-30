(* Configure using the newer "atomic" API. *)

module K = Drm.Kms

let println fmt = Fmt.pr (fmt ^^ "@.")

let find_plane dev ~crtc_idx =
  let compatible plane_id =
    let plane = K.Plane.get dev plane_id in
    if plane.possible_crtcs land (1 lsl crtc_idx) <> 0 then (
      let props = K.Plane.get_properties dev plane_id in
      match K.Properties.get_value props K.Plane.typ with
      | Some `Primary -> Some props
      | _ -> None
    ) else None
  in
  K.Plane.list dev |> List.find_map compatible

let show_test_page (t : Resources.t) rq (c : K.Connector.t) =
  let obj = K.Connector.get_properties t.dev c.connector_id in
  match K.Properties.get_value_exn obj K.Connector.crtc_id with
  | None -> Fmt.failwith "Connector %a has no CRTC" Drm.Id.pp c.connector_id
  | Some crtc_id ->
    println "Preparing settings for %a" K.Connector.pp_name c;
    let mode = List.hd c.modes in
    let size = (mode.hdisplay, mode.vdisplay) in
    let crtc_idx = Resources.crtc_index t crtc_id in
    let plane =
      match find_plane t.dev ~crtc_idx with
      | Some x -> x
      | None ->
        Fmt.failwith "No suitable primary plane for CRTC %a (with index %d)"
          Drm.Id.pp crtc_id
          crtc_idx
    in
    println "Using plane %a" Drm.Id.pp (K.Properties.object_id plane);     (* todo: already used? *)
    let fb = Test_image.create t.dev size in
    K.Atomic_req.add_property rq plane K.Plane.fb_id (Some fb)

let () =
  Utils.with_device @@ fun t ->
  Drm.Client_cap.(set_exn atomic) t.dev true;
  let connected = List.filter Utils.is_connected t.connectors in
  println "Found %d connected connectors" (List.length connected);
  let rq = K.Atomic_req.create () in
  List.iter (show_test_page t rq) connected;
  println "Checking that commit will work...";
  match K.Atomic_req.commit ~test_only:true t.dev rq with
  | exception Unix.Unix_error (code, "drmModeAtomicCommit", _) ->
    println "Mode-setting would fail with error: %s" (Unix.error_message code)
  | () ->
    println "Pre-commit test passed.";
    Utils.restore_modes_on_exit t @@ fun () ->
    K.Atomic_req.commit t.dev rq;
    Unix.sleep 2
