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
val partition : ('a -> int) -> 'a array -> 'a array array

val int_seq   : min:int   -> max:int   -> incr:int   -> int array
val float_seq : min:float -> max:float -> incr:float -> float array

val sorted_copy      : ?compare:('a -> 'a -> int) -> 'a array -> 'a array 
val fast_sorted_copy : ?compare:('a -> 'a -> int) -> 'a array -> 'a array

val for_all : (int -> 'a -> bool) -> 'a array -> bool
val exists  : (int -> 'a -> bool) -> 'a array -> bool
val lexists : (int -> 'a -> bool) -> 'a array -> int option
val rexists : (int -> 'a -> bool) -> 'a array -> int option

val search  : ('a -> bool) -> 'a array -> 'a option
val searchi : ('a -> bool) -> 'a array -> (int * 'a) option
val find    : ('a -> bool) -> 'a array -> 'a
val findi   : ('a -> bool) -> 'a array -> (int * 'a)

val shared_property : ('a -> 'b) -> 'a array -> bool

val dichotomic_search : 'a array -> 'a -> bool * int
val dichotomic_insert : 'a array -> 'a -> 'a array

val for_all2 : (int -> 'a -> 'b -> bool) -> 'a array -> 'b array -> bool
val exists2  : (int -> 'a -> 'b -> bool) -> 'a array -> 'b array -> bool

val iter2  : ('a -> 'b -> unit) -> 'a array -> 'b array -> unit
val iteri2 : (int -> 'a -> 'b -> unit) -> 'a array -> 'b array -> unit

val map2   : ('a -> 'b -> 'c) -> 'a array -> 'b array -> 'c array
val mapi2  : (int -> 'a -> 'b -> 'c) -> 'a array -> 'b array -> 'c array

val fold_lefti  : (int -> 'a -> 'b -> 'a) -> 'a -> 'b array -> 'a
val fold_righti : (int -> 'a -> 'b -> 'b) -> 'a array -> 'b -> 'b

val fold_left2 : ('a -> 'b -> 'c -> 'a) -> 'a -> 'b array -> 'c array -> 'a
val fold_right2 : ('a -> 'b -> 'c -> 'c) -> 'a array -> 'b array -> 'c -> 'c

val fold_lefti2 : (int -> 'a -> 'b -> 'c -> 'a) -> 'a -> 'b array -> 'c array -> 'a
val fold_righti2 : (int -> 'a -> 'b -> 'c -> 'c) -> 'a array -> 'b array -> 'c -> 'c

val init2 : int -> (int -> 'a *'b) -> 'a array * 'b array
val split : ('a * 'b) array -> 'a array * 'b array
  
module Matrix : sig
 type 'a t = 'a array array
 val init : int -> int -> (int -> int -> 'a) -> 'a t
 val of_list : 'a list list -> 'a t
 val to_list : 'a t -> 'a list list
 val transpose : 'a t -> 'a t
end
