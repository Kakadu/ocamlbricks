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

(** Additional features for the standard module [Array].*)

val of_known_length_list : ?reversing:bool -> int -> 'a list -> 'a array

val for_all : (int -> 'a -> bool) -> 'a array -> bool
val exists  : (int -> 'a -> bool) -> 'a array -> bool
val lexists : (int -> 'a -> bool) -> 'a array -> int option
val rexists : (int -> 'a -> bool) -> 'a array -> int option

val dichotomic_search : 'a array -> 'a -> bool * int
val dichotomic_insert : 'a array -> 'a -> 'a array

module Matrix : sig
 type 'a t = 'a array array
 val init : int -> int -> (int -> int -> 'a) -> 'a t
 val of_list : 'a list list -> 'a t
 val to_list : 'a t -> 'a list list
 val transpose : 'a t -> 'a t
end
