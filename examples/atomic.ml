(* Configure using the newer "atomic" API. *)

module Kms = Drm.Kms

let println fmt = Fmt.pr (fmt ^^ "@.")

let find_plane dev ~crtc_idx =
  let compatible plane_id =
    let plane = Kms.Plane.get dev plane_id in
    if plane.possible_crtcs land (1 lsl crtc_idx) <> 0 then (
      let props = Kms.Plane.get_properties dev plane_id in
      match Kms.Properties.get_value props Kms.Plane.typ with
      | Some `Primary -> Some props
      | _ -> None
    ) else None
  in
  Kms.Plane_resources.get dev |> List.find_map compatible

let show_test_page (t : Resources.t) tx (c : Kms.Connector.t) =
  let obj = Kms.Connector.get_properties t.dev c.connector_id in
  match Kms.Properties.get_value_exn obj Kms.Connector.crtc_id with
  | None -> Fmt.failwith "Connector %a has no CRTC" Drm.Id.pp c.connector_id
  | Some crtc_id ->
    println "Preparing settings for %a" Kms.Connector.pp_name c;
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
    println "Using plane %a" Drm.Id.pp (Kms.Properties.object_id plane);     (* todo: already used? *)
    let fb = Test_image.create t.dev size in
    Kms.Atomic_req.add_property tx plane Kms.Plane.fb_id (Some fb)

let () =
  Utils.with_device @@ fun t ->
  Drm.Client_cap.(set_exn atomic) t.dev true;
  let connected = List.filter Utils.is_connected t.connectors in
  println "Found %d connected connectors" (List.length connected);
  let tx = Kms.Atomic_req.create () in
  List.iter (show_test_page t tx) connected;
  println "Checking that commit will work...";
  match Kms.Atomic_req.commit ~test_only:true t.dev tx with
  | exception Unix.Unix_error (code, "drmModeAtomicCommit", _) ->
    println "Mode-setting would fail with error: %s" (Unix.error_message code)
  | () ->
    println "Pre-commit test passed.";
    Utils.restore_modes_on_exit t @@ fun () ->
    Kms.Atomic_req.commit t.dev tx;
    Unix.sleep 2
