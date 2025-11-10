module K = Drm.Kms

let println fmt = Fmt.pr (fmt ^^ "@.")

let open_device (d : Drm.Device.Info.t) =
  match d.primary_node with
  | None -> None
  | Some primary ->
    let dev = Unix.openfile primary [O_RDWR; O_CLOEXEC] 0 in
    if Drm.Device.is_kms dev then Some dev
    else (
      Unix.close dev;
      None
    )

let test_fixed () =
  let module T = Drm.Ufixed in
  let assert_equal a b =
    let a = Unsigned.UInt32.of_int32 a in
    let b = T.to_bits b in
    if a <> b then Fmt.failwith "%a <> %a!" Unsigned.UInt32.pp_hex a Unsigned.UInt32.pp_hex b
  in
  let assert_inval fn x =
    try ignore (fn x : T.t); assert false
    with Invalid_argument _ -> ()
  in
  assert_equal 0l @@ T.of_int 0;
  assert_equal 0l @@ T.of_float 0.;
  assert_equal 0x10000l @@ T.of_int 1;
  assert_equal 0x10000l @@ T.of_float 1.;
  assert_equal 0xffff0000l @@ T.of_int 0xffff;
  assert_equal 0xffff0000l @@ T.of_float 0xffff.;
  assert_equal 0xffffffffl @@ T.of_float 0xffff.ffff;
  assert_equal 0xffffffffl @@ T.of_float 0xffff.fffff;
  assert_equal 0x18000l @@ T.of_float 1.5;
  assert_inval T.of_int (-1);
  assert_inval T.of_int 0x10000;
  assert_inval T.of_float 0x10000.;
  assert_inval T.of_float (-1.);
  assert (T.to_float (T.of_int 0xffff) = 0xffff.);
  ()

let test_blob dev =
  let id = K.Blob.create dev "hello" in
  assert (K.Blob.get dev id = "hello");
  K.Blob.destroy dev id;
  try ignore (K.Blob.get dev id); assert false
  with Unix.Unix_error (ENOENT, _, _) -> ()

let () =
  test_fixed ();
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
      Leases.test dev;
      test_blob dev
