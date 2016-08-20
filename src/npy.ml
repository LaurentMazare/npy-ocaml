exception Cannot_write

let magic_string = "\147NUMPY"
let magic_string_len = String.length magic_string

let dtype (type a b) (bigarray : (a, b, _) Bigarray.Genarray.t) =
  let endianness =
    if Sys.big_endian
    then ">"
    else "<"
  in
  let kind =
    match Bigarray.Genarray.kind bigarray with
    | Bigarray.Int32 -> "i4"
    | Bigarray.Int64 -> "i8"
    | Bigarray.Float32 -> "f4"
    | Bigarray.Float64 -> "f8"
    | _ -> failwith "Not supported yet."
  in
  endianness ^ kind

let fortran_order (type a) (bigarray : (_, _, a) Bigarray.Genarray.t) =
  match Bigarray.Genarray.layout bigarray with
  | Bigarray.C_layout -> "False"
  | Bigarray.Fortran_layout -> "True"

let shape bigarray =
  Bigarray.Genarray.dims bigarray
  |> Array.to_list
  |> List.map string_of_int
  |> String.concat ", "

let full_header bigarray =
  let header =
    Printf.sprintf
      "{'descr': '%s', 'fortran_order': %s, 'shape': (%s), }"
      (dtype bigarray)
      (fortran_order bigarray)
      (shape bigarray)
  in
  let padding_len =
    let total_len = String.length header + magic_string_len + 4 + 1 in
    if total_len mod 16 = 0
    then 0
    else 16 - total_len mod 16
  in
  let total_header_len = String.length header + padding_len + 1 in
  Printf.sprintf "%s\001\000%c%c%s%s\n"
    magic_string
    (total_header_len mod 256 |> Char.chr)
    (total_header_len / 256 |> Char.chr)
    header
    (String.make padding_len ' ')

let write bigarray filename =
  let file_descr = Unix.openfile filename [ O_CREAT; O_TRUNC; O_RDWR ] 0o640 in
  let full_header = full_header bigarray in
  let full_header_len = String.length full_header in
  if Unix.write file_descr full_header 0 full_header_len <> full_header_len
  then raise Cannot_write;
  let file_array =
    Bigarray.Genarray.map_file
      ~pos:(Int64.of_int full_header_len)
      file_descr
      (Bigarray.Genarray.kind bigarray)
      (Bigarray.Genarray.layout bigarray)
      true
      (Bigarray.Genarray.dims bigarray)
  in
  Bigarray.Genarray.blit bigarray file_array;
  Unix.close file_descr

let write1 array1 filename =
  write (Bigarray.genarray_of_array1 array1) filename

let write2 array2 filename =
  write (Bigarray.genarray_of_array2 array2) filename

let write3 array3 filename =
  write (Bigarray.genarray_of_array3 array3) filename
