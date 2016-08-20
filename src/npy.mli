val write1 : ('a, 'b, 'c) Bigarray.Array1.t -> string -> unit
val write2 : ('a, 'b, 'c) Bigarray.Array2.t -> string -> unit
val write3 : ('a, 'b, 'c) Bigarray.Array3.t -> string -> unit
val write : ('a, 'b, 'c) Bigarray.Genarray.t -> string -> unit

type packed_array = P : (_, _, _) Bigarray.Genarray.t -> packed_array

(** [read_mmap filename ~shared] returns a packed bigarray mmaped to the content
    of [filename]. If [shared] is [true] modifications made to the array are reflected
    to the file. *)
val read_mmap : string -> shared:bool -> packed_array

val read_copy : string -> packed_array
