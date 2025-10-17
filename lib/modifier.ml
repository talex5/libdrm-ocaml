type t = int64

let of_int64 t = t
let of_uint64 = Unsigned.UInt64.to_int64
let to_uint64 = Unsigned.UInt64.of_int64

let pp f t = Fmt.pf f "%Lx" t

let reserved = 0xffffffffffffffL
let linear = 0L
