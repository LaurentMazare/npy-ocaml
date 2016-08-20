let save_dummy_array filename =
  let dim1 = 2 in
  let dim2 = 3 in
  let bigarray =
    Bigarray.Array2.create
      Float32
      C_layout
      dim1
      dim2
  in
  for idx1 = 0 to dim1 - 1 do
    for idx2 = 0 to dim2 - 1 do
      Bigarray.Array2.set bigarray idx1 idx2 (idx1 + idx2 * dim1 |> float)
    done;
  done;
  Npy.write (Bigarray.genarray_of_array2 bigarray) filename;
  let content = Npy.read_only_mmap filename in
  ignore content

let () =
  save_dummy_array "test.npy"
