let println fmt = Fmt.pr (fmt ^^ "@.")

let open_device (d : Drm.Device.Info.t) =
  match d.primary_node with
  | None -> None
  | Some primary ->
    let fd = Unix.openfile primary [O_RDWR; O_CLOEXEC] 0 in
    if Drm.Device.is_kms fd then Some fd
    else (
      Unix.close fd;
      None
    )

let () =
  match Drm.Device.list ~get_pci_revision:true () with
  | exception Unix.Unix_error (ENOENT, _, _) ->
    println "DRM not supported on this platform; skipping tests"
  | devices ->
    println "@[<v2>devices:@,%a@]" (Fmt.Dump.list Drm.Device.Info.pp) devices;
    match List.find_map open_device devices with
    | None -> println "No suitable device found; skipping tests"
    | Some dev ->
      Buffers.test_dumb_buffer dev;
      Events.test_sync dev;
