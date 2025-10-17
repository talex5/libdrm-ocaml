external caml_unix_error_of_code : int -> Unix.error = "caml_unix_error_of_code_2"

let code_of_errno errno =
  caml_unix_error_of_code (Signed.SInt.to_int errno)

let report errno fn arg =
  raise (Unix.Unix_error (code_of_errno errno, fn, arg))

(* For functions that return e.g. [-EINVAL] rather than using errno. *)
let report_neg code fn arg =
  let code = caml_unix_error_of_code (-code) in
  raise (Unix.Unix_error (code, fn, arg))

let ignore (x, (_ : Signed.SInt.t)) = x
