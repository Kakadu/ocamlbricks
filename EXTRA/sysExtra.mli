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

(** Additional features for the standard module [Sys]. *)

(** {2 Reading directories } *)

val readdir_as_list :
  ?only_directories:unit -> 
  ?only_not_directories:unit ->
  ?name_filter:(string -> bool) ->
  ?name_converter:(string -> string) ->
  string -> string list

(** {2 Rewriting files } *)

val put : ?callback:(string -> unit) -> string -> string -> unit

(** {2 Signals} *)

val int_of_signal  : int -> int
val name_of_signal : int -> string
val description_of_signal : int -> string * string * string * string
val description_of_name   : string -> string * string * string

val signal_behavior : int -> Sys.signal_behavior
val iter_on_signals : ?except:(int list) -> (int -> Sys.signal_behavior -> unit) -> unit
val fold_on_signals : ?except:(int list) -> ('a -> int -> Sys.signal_behavior -> 'a) -> 'a -> 'a

val log_signal_reception : ?except:(int list) -> unit -> unit

IFDEF DOCUMENTATION_OR_DEBUGGING THEN
val test_log_signal_reception : unit -> unit
ENDIF
