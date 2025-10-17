include Types_generated.Cap

let ( !@ ) = Ctypes.( !@ )

let get (type a) ((ty : a ty), cap) fd : (a, Unix.error) result =
  let value = Ctypes.allocate C.Types.int_uint64 0 in
  let err, errno = C.Functions.drmGetCap fd cap value in
  if err = 0 then (
    let v = !@ value in
    match ty with
    | Int -> Ok v
    | Bool -> Ok (v <> 0)
  ) else (
    Error (Err.code_of_errno errno)
  )

let get_exn t fd =
  match get t fd with
  | Ok x -> x
  | Error code -> raise (Unix.Unix_error (code, "drmGetCap", ""))
