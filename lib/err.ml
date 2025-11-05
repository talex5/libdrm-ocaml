external caml_unix_error_of_code : int -> Unix.error = "caml_unix_error_of_code_2"

let error_of_errno errno =
  caml_unix_error_of_code (Signed.SInt.to_int errno)

let error_of_neg code =
  assert (code < 0);
  caml_unix_error_of_code (-code)

let report errno fn arg =
  raise (Unix.Unix_error (error_of_errno errno, fn, arg))

(* For functions that return e.g. [-EINVAL] rather than using errno. *)
let report_neg x fn arg =
  raise (Unix.Unix_error (error_of_neg x, fn, arg))

let ignore (x, (_ : Signed.SInt.t)) = x
