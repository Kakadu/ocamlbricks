type t

val create       : ?block_size:int -> unit            -> t
val from_descr   : ?block_size:int -> Unix.file_descr -> t
val from_file    : ?block_size:int -> string          -> t
val from_channel : ?block_size:int -> in_channel      -> t

val append_from_descr : ?release:bool -> t -> Unix.file_descr -> unit

type blit_function = string -> int -> string -> int -> int -> unit
val  concat : ?blit:blit_function -> t -> string

module Thread_unsafe :
 sig
  val append_from_descr : ?release:bool -> t -> Unix.file_descr -> unit
  val concat            : ?blit:blit_function -> t -> string
 end
