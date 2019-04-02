let print_one filename =
  let npz_in = Npy.Npz.open_in filename in
  let entries = Npy.Npz.entries npz_in in
  Printf.printf "%s:\n" filename;
  List.iter
    (fun entry ->
      match Npy.Npz.read npz_in entry with Npy.P array ->
        Bigarray.Genarray.dims array
        |> Array.to_list
        |> List.map string_of_int
        |> String.concat ", "
        |> Printf.printf "  %s (%s)\n" entry )
    entries;
  Npy.Npz.close_in npz_in

let () =
  match Array.to_list Sys.argv with
  | [] -> Printf.printf "Usage: %s file1.npz file2.npz...\n" Sys.argv.(0)
  | _ :: argv -> List.iter print_one argv
