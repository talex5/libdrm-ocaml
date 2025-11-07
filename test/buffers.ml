module K = Drm.Kms

let println fmt = Fmt.pr (fmt ^^ "@.")

let test_sync_file dmabuf_fd =
  let sync_fd = Drm.Dmabuf.export_sync_file dmabuf_fd `RW in
  Drm.Dmabuf.import_sync_file dmabuf_fd ~sync_file_fd:sync_fd `RW;
  Unix.close sync_fd

let test_dumb_buffer dev =
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
