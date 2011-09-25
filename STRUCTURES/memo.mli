(* This file is part of our reusable OCaml BRICKS library
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

(** Simple function memoization.*)

val default_size : int

(** Transform a function into its memoized version. *)
val memoize : ?trace_faults:unit -> ?size:int -> ('a -> 'b) -> 'a -> 'b

(** In order to manually manage the memory allocation it may be useful to get also the hash table: *)
val memoize_and_get_table : ?trace_faults:unit -> ?size:int -> ('a -> 'b) -> ('a -> 'b) * ('a,'b) Hashtbl.t
