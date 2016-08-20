val write1 : ('a, 'b, 'c) Bigarray.Array1.t -> string -> unit
val write2 : ('a, 'b, 'c) Bigarray.Array2.t -> string -> unit
val write3 : ('a, 'b, 'c) Bigarray.Array3.t -> string -> unit
val write : ('a, 'b, 'c) Bigarray.Genarray.t -> string -> unit

type packed_array = P : (_, _, _) Bigarray.Genarray.t -> packed_array
val read_only_mmap : string -> packed_array
