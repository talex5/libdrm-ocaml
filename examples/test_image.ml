let create dev size =
  let dumb_buffer = Drm.Buffer.Dumb.create dev ~bpp:32 ~size in
  let arr = Drm.Buffer.Dumb.map dev dumb_buffer Int32 size in
  for row = 0 to snd size - 1 do
    for col = 0 to fst size - 1 do
      let c =
        (row land 0xff) lor
        ((col land 0xff) lsl 8) lor
        (((row lsr 8) lor (col lsr 8)) lsl 18)
      in
      arr.{row, col} <- Int32.of_int c
    done;
  done;
  dumb_buffer
