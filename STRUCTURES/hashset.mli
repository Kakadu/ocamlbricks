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

(** Polymorphic {e unbounded} sets.
    An encapsulated [('a, unit) Hashtbl.t] is used for quickly answering
    to the membership problem.  *)

type 'a t

val make       : ?size:int -> unit -> 'a t
val mem        : 'a t -> 'a -> bool
val add        : 'a t -> 'a -> unit
val remove     : 'a t -> 'a -> unit
val of_list    : 'a list -> 'a t
val to_list    : 'a t -> 'a list
val of_array   : 'a array -> 'a t
val to_array   : 'a t -> 'a array 
val list_uniq  : 'a list -> 'a list 
val array_uniq : 'a array -> 'a array
val uniq       : 'a list -> 'a list (* alias for list_uniq *)

(** {2 Object-oriented interface} *)

class ['a] hashset :
  ?size:int ->
  unit ->
  object
    method hashtbl : ('a, unit) Hashtbl.t
    method add     : 'a -> unit
    method mem     : 'a -> bool
    method remove  : 'a -> unit
  end
