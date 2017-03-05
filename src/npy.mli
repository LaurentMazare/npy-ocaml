val write1 : ('a, 'b, 'c) Bigarray.Array1.t -> string -> unit
val write2 : ('a, 'b, 'c) Bigarray.Array2.t -> string -> unit
val write3 : ('a, 'b, 'c) Bigarray.Array3.t -> string -> unit
val write : ('a, 'b, 'c) Bigarray.Genarray.t -> string -> unit

module Batch_writer : sig
  type t

  val create
    :  string
    -> t

  val append
    :  t
    -> (_, _, Bigarray.c_layout) Bigarray.Genarray.t
    -> unit

  val close : t -> unit
end

type packed_array = P : (_, _, _) Bigarray.Genarray.t -> packed_array
type packed_array1 = P1 : (_, _, _) Bigarray.Array1.t -> packed_array1
type packed_array2 = P2 : (_, _, _) Bigarray.Array2.t -> packed_array2
type packed_array3 = P3 : (_, _, _) Bigarray.Array3.t -> packed_array3

(** [read_mmap filename ~shared] returns a packed bigarray mmaped to the content
    of [filename]. If [shared] is [true] modifications made to the array are reflected
    to the file. *)
val read_mmap : string -> shared:bool -> packed_array
val read_mmap1: string -> shared:bool -> packed_array1
val read_mmap2: string -> shared:bool -> packed_array2
val read_mmap3: string -> shared:bool -> packed_array3

val read_copy : string -> packed_array
val read_copy1 : string -> packed_array1
val read_copy2 : string -> packed_array2
val read_copy3 : string -> packed_array3

module Npz : sig
  type in_file
  val open_in : string -> in_file
  val read : ?suffix:string -> in_file -> string -> packed_array
  val close_in : in_file -> unit

  type out_file
  val open_out : string -> out_file
  val write : ?suffix:string -> out_file -> string -> ('a, 'b, 'c) Bigarray.Genarray.t -> unit
  val close_out : out_file -> unit
end
