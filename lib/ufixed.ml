type t = Unsigned.UInt32.t

let of_bits x = x
let to_bits x = x

let of_int x =
  if x land 0xffff = x then Unsigned.UInt32.(shift_left (of_int x) 16)
  else Fmt.invalid_arg "%d out-of-range for a 16.16 fixed value" x

let of_float x =
  if x < 0. || x >= 0x10000.0 then Fmt.invalid_arg "%g out-of-range for a 16.16 fixed value" x
  else Unsigned.UInt64.to_uint32 (Unsigned.UInt64.of_int64 (Int64.of_float (x *. 0x10000.0)))

let to_float t = Int64.to_float (Unsigned.UInt32.to_int64 t) /. 0x10000.

let pp f t =
  Fmt.pf f "%g" (to_float t)
