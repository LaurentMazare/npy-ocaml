let print_array2 array2 =
  let dim1 = Bigarray.Array2.dim1 array2 in
  let dim2 = Bigarray.Array2.dim2 array2 in
  for idx1 = 0 to dim1 - 1 do
    for idx2 = 0 to dim2 - 1 do
      Printf.printf "%d %d => %f\n" idx1 idx2 array2.{idx1, idx2}
    done;
  done

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
  Npy.write2 bigarray filename;
  let Npy.P array2 = Npy.read_only_mmap filename in
  let array2 = Bigarray.array2_of_genarray array2 in
  match Bigarray.Array2.kind array2 with
  | Bigarray.Float32 -> print_array2 array2
  | _ -> assert false

let () =
  save_dummy_array "test.npy"
