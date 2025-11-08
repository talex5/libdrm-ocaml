module K = Drm.Kms

let println fmt = Fmt.pr (fmt ^^ "@.")

let active_crtc (x : K.Crtc.t) = x.mode <> None

let test_sync dev =
  let mode_res = K.Resources.get dev in
  let crtcs = List.map (K.Crtc.get dev) mode_res.crtcs in
  match List.find_opt active_crtc crtcs with
  | None -> println "No active CRTCs; skipping event test"
  | Some crtc ->
    let seq = K.Crtc.queue_sequence dev crtc.crtc_id (`Relative 1) ~user_data:42n in
    println "Queued event for sequence %a" Unsigned.UInt64.pp seq;
    let buffer = Drm.Event.create_buffer () in
    let got = Unix.read_bigarray dev buffer 0 (Bigarray.Array1.dim buffer) in
    let events = Drm.Event.parse buffer got in
    println "Events: %a" (Fmt.Dump.list Drm.Event.pp) events
