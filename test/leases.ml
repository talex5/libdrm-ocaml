module K = Drm.Kms

let println fmt = Fmt.pr (fmt ^^ "@.")

let pp_grant f (K.Lease.Grant id) = Drm.Id.pp f id

let test dev =
  Drm.Client_cap.(set_exn universal_planes) dev true;
  let resources = K.Resources.get dev in
  let planes = K.Plane.list dev in
  if not (Drm.Device.is_master dev) then (
    println "Leases: Not master; skipping test"
  ) else (
    match resources, planes with
    | { crtcs = crtc :: _; connectors = conn :: _; _ }, plane :: _ ->
      let granted = K.Lease.[Grant crtc; Grant conn; Grant plane] in
      let id, sub_dev = K.Lease.create dev granted in
      println "Created lease %a" Drm.Id.pp id;
      let r2 = K.Resources.get sub_dev in
      println "Full: %a@,Sub: %a" K.Resources.pp resources K.Resources.pp r2;
      assert (K.Lease.list_lessees dev = [id]);
      let leased = K.Lease.get_lease sub_dev in
      println "Objects: %a" (Fmt.Dump.list pp_grant) leased;
      assert (List.sort compare leased = List.sort compare granted);
      K.Lease.revoke dev id;
      assert (K.Lease.get_lease sub_dev = []);
      assert (K.Lease.list_lessees dev = []);
      Unix.close sub_dev
    | _ -> println "Not enough resources to make a lease"
  )
