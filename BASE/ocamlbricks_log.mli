(* This file is part of ocamlbricks
   Copyright (C) 2011 Jean-Vincent Loddo

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

val enable  : unit -> unit
val disable : unit -> unit

val printf        : ?v:int -> ?force:bool -> ?banner:bool -> ('a, out_channel, unit) format -> 'a
val print_exn     : ?v:int -> ?force:bool -> ?banner:bool -> exn -> unit
val print_string  : ?v:int -> ?force:bool -> string -> unit
val print_int     : ?v:int -> ?force:bool -> int -> unit
val print_float   : ?v:int -> ?force:bool -> float -> unit
val print_newline : ?v:int -> ?force:bool -> unit -> unit
val print_endline : ?v:int -> ?force:bool -> string -> unit

module Tuning :
  sig
    val verbosity      : unit -> int
    val debug_level    : unit -> int
    val is_log_enabled : ?v:int -> unit -> bool
    val log_channel    : Log_builder.log_channel
    val synchronized   : bool

    module Set :
      sig
        val verbosity   : int -> unit
        val debug_level : (unit -> int) -> unit
      end
  end
