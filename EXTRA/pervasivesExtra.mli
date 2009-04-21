(** Additional features for the standard module [Pervasives]. *)

(** {b Print and flush.} Print something onto stdout, then immediately flush the buffer. This may be
    slower but allows to print without waiting for automatic flushes, which are
    very infrequent when more than one process is concurrently writing to the
    same channel. *)

val print_char    : char   -> unit
val print_string  : string -> unit
val print_int     : int    -> unit
val print_float   : float  -> unit
val print_endline : string -> unit
val print_newline : unit   -> unit

(** {2 Make datatype printers} *)

module Printers0 :
  functor (M : sig type t val string_of : t -> string end) ->
    sig
      type t = M.t
      val string_of : t -> string
      val print         : M.t -> unit
      val prerr         : M.t -> unit
      val print_endline : M.t -> unit
      val prerr_endline : M.t -> unit
      val fprintf :
        out_channel -> (string -> 'a, out_channel, unit) format -> M.t -> 'a
      val eprintf :    (string -> 'a, out_channel, unit) format -> M.t -> 'a
      val printf  :    (string -> 'a, out_channel, unit) format -> M.t -> 'a
      val sprintf :    (string -> 'a, unit, string) format      -> M.t -> 'a
    end

module Printers1 :
  functor
    (M : sig type 'a t val string_of : ('a -> string) -> 'a t -> string end) ->
    sig
      type 'a t = 'a M.t
      val string_of     : ('a -> string) -> 'a t -> string
      val print         : ('a -> string) -> 'a M.t -> unit
      val prerr         : ('a -> string) -> 'a M.t -> unit
      val print_endline : ('a -> string) -> 'a M.t -> unit
      val prerr_endline : ('a -> string) -> 'a M.t -> unit
      val fprintf :       ('a -> string) -> out_channel -> (string -> 'b, out_channel, unit) format -> 'a M.t -> 'b
      val eprintf :       ('a -> string) ->                (string -> 'b, out_channel, unit) format -> 'a M.t -> 'b
      val printf :        ('a -> string) ->                (string -> 'b, out_channel, unit) format -> 'a M.t -> 'b
      val sprintf :       ('a -> string) ->                (string -> 'b, unit, string) format      -> 'a M.t -> 'b
    end
