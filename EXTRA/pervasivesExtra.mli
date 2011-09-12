(* This file is part of our reusable OCaml BRICKS library
   Copyright (C) 2009 Jean-Vincent Loddo

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>. *)

(** Additional features for the standard module [Pervasives]. *)

val round : ?decimals:int -> float -> float

val for_float : ?backward:unit -> min:float -> max:float -> step:float -> ('a -> float -> 'a) -> 'a -> 'a
val for_int   : ?backward:unit -> ?step:int -> min:int -> max:int -> ('a -> int -> 'a) -> 'a -> 'a
  
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
