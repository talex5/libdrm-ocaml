include Types_generated.Client_cap

let set cap fd value =
  let value =
    if value then (
      if cap = atomic then 2  (* "The modesetting DDX has a totally broken idea of atomic" *)
      else 1
    ) else 0
  in
  match C.Functions.drmSetClientCap fd cap (Unsigned.UInt64.of_int value) with
  | 0, _ -> Ok ()
  | _, errno -> Error (Err.code_of_errno errno)

let set_exn cap fd value =
  match set cap fd value with
  | Ok x -> x
  | Error code -> raise (Unix.Unix_error (code, "drmSetClientCap", ""))
