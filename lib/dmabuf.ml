type t = Unix.file_descr
type sync_file = Unix.file_descr

let ( !@ ) = Ctypes.( !@ )

let drm_ioctl fd op arg =
  match C.Functions.drmIoctl fd op (Ctypes.to_voidp (Ctypes.addr arg)) with
  | 0, _ -> ()
  | _, errno -> Err.report errno "drmIoctl" ""

let of_handle ~rw dev handle =
  let prime_fd = Ctypes.(allocate C.Types.fd Unix.stdin) in
  let flags = C.Types.Dmabuf.cloexec in
  let flags = if rw then Unsigned.UInt32.logor flags C.Types.Dmabuf.rdwr else flags in
  let err, errno = C.Functions.drmPrimeHandleToFD dev handle flags prime_fd in
  if err = 0 then !@ prime_fd
  else Err.report errno "drmPrimeHandleToFD" ""

let to_handle dev dmabuf =
  let handle = Ctypes.(allocate_n C.Types.buffer_id ~count:1) in
  let err, errno = C.Functions.drmPrimeFDToHandle dev dmabuf handle in
  if err = 0 then !@ handle
  else Err.report errno "drmPrimeFDToHandle" ""

let export_sync_file dmabuf `RW =
  let module CT = C.Types.Dma_buf_export_sync_file in
  let c = Ctypes.make CT.t in
  (* Note: [dma_buf_export_sync_file] always uses O_CLOEXEC for the new FD *)
  Ctypes.setf c CT.flags C.Types.Dmabuf.sync_rw;
  Ctypes.setf c CT.fd Unix.stdin;
  drm_ioctl dmabuf C.Types.Dmabuf.ioctl_export_sync_file c;
  Ctypes.getf c CT.fd

let import_sync_file dmabuf ~sync_file_fd `RW =
  let module CT = C.Types.Dma_buf_import_sync_file in
  let c = Ctypes.make CT.t in
  Ctypes.setf c CT.flags C.Types.Dmabuf.sync_rw;
  Ctypes.setf c CT.fd sync_file_fd;
  drm_ioctl dmabuf C.Types.Dmabuf.ioctl_import_sync_file c
