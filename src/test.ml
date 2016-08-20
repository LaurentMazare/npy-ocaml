let print_array2 array2 to_string =
  let dim1 = Bigarray.Array2.dim1 array2 in
  let dim2 = Bigarray.Array2.dim2 array2 in
  for idx1 = 0 to dim1 - 1 do
    for idx2 = 0 to dim2 - 1 do
      Printf.printf "%d %d => %s\n" idx1 idx2 (to_string array2.{idx1, idx2})
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
  let Npy.P array2 = Npy.read_mmap filename ~shared:false in
  let array2 = Bigarray.array2_of_genarray array2 in
  match Bigarray.Array2.kind array2 with
  | Bigarray.Float32 -> print_array2 array2 string_of_float
  | Bigarray.Float64 -> print_array2 array2 string_of_float
  | Bigarray.Int32 -> print_array2 array2 Int32.to_string
  | Bigarray.Int64 -> print_array2 array2 Int64.to_string
  | _ -> assert false

let () =
  save_dummy_array "test.npy"
