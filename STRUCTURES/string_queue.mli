(** Thread safe, efficient concatenation string queues. *)

type t

val create       : ?block_size:int -> unit            -> t

(** {2 Writers' tools} *)

(** The queue is released by default for all operations. The user could request
    the non-releasing setting the optional parameter [~release] to [false]. *)

val append_from_descr : ?release:bool -> t -> Unix.file_descr -> unit

val from_descr   : ?release:bool -> ?block_size:int -> Unix.file_descr -> t
val from_file    : ?release:bool -> ?block_size:int -> string          -> t
val from_channel : ?release:bool -> ?block_size:int -> in_channel      -> t

(** {2 Readers' tools} *)

type blit_function = string -> int -> string -> int -> int -> unit
val  concat : ?blit:blit_function -> t -> string

(** {2 Thread_unsafe versions} *)

module Thread_unsafe :
 sig
  val append_from_descr : ?release:bool -> t -> Unix.file_descr -> unit
  val concat            : ?blit:blit_function -> t -> string
 end
