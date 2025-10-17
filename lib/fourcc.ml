type t = int32

let of_int32 t = t
let of_uint32 = Unsigned.UInt32.to_int32
let to_uint32 = Unsigned.UInt32.of_int32

let rev_if = function
  | true -> List.rev
  | false -> Fun.id

let to_string t =
  List.init 4 (fun i -> Char.chr @@ Int32.to_int @@ Int32.logand 0xffl @@ Int32.shift_right_logical t (i * 8))
  |> rev_if Sys.big_endian
  |> List.to_seq
  |> String.of_seq

let of_string s =
  String.to_seq s
  |> List.of_seq
  |> rev_if @@ not Sys.big_endian
  |> List.fold_left (fun acc i -> Int32.logor (Int32.of_int @@ Char.code i) @@ Int32.shift_left acc 8) 0l

let pp = Fmt.of_to_string to_string

let xr24 = of_string "XR24"
