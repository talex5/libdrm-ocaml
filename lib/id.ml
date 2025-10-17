type 'a t = int

let of_int = function
  | 0 -> invalid_arg "IDs cannot be 0"
  | x -> x

let of_uint32 x = of_int (Unsigned.UInt32.to_int x)
let of_uint64 x = of_int (Unsigned.UInt64.to_int x)

let of_uint32_opt x =
  match Unsigned.UInt32.to_int x with
  | 0 -> None
  | t -> Some t

let to_uint32 = Unsigned.UInt32.of_int
let to_uint64 = Unsigned.UInt64.of_int

let to_uint64_opt = function
  | None -> Unsigned.UInt64.zero
  | Some t -> to_uint64 t

let of_int64 x = of_int (Int64.to_int x)

let of_uint64_opt x =
  if x = Unsigned.UInt64.zero then None
  else Some (of_uint64 x)

let pp = Fmt.int
