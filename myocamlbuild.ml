open Ocamlbuild_plugin
let () =
  dispatch (function
    | After_rules ->
      ocaml_lib "src/npy"
    | _ -> ())

