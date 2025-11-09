include Types_generated.Client_cap

let set cap dev value =
  let value =
    if value then (
      if cap = atomic then 2  (* "The modesetting DDX has a totally broken idea of atomic" *)
      else 1
    ) else 0
  in
  match C.Functions.drmSetClientCap dev cap (Unsigned.UInt64.of_int value) with
  | 0, _ -> Ok ()
  | _, errno -> Error (Err.error_of_errno errno)

let set_exn cap dev value =
  match set cap dev value with
  | Ok x -> x
  | Error code -> raise (Unix.Unix_error (code, "drmSetClientCap", ""))
