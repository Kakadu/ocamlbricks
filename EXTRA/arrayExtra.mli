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
val partition  : ?min_size:int -> ('a -> int) -> 'a array -> 'a array array
val partitioni : ?min_size:int -> (int -> 'a -> int) -> 'a array -> 'a array array
val amass      : ?group_no:int -> ?size:int -> 'a array -> 'a array array
val group_by   : ('a -> 'b * 'c) -> 'a array -> 'b array * ('b, 'c array) Hashtbl.t
val flatten    : 'a array array -> 'a array
val sub        : ?len:int -> 'a array -> int -> 'a array

val int_seq   : min:int   -> max:int   -> incr:int   -> int array
val float_seq : min:float -> max:float -> incr:float -> float array

val sorted_copy             : ?compare:('a -> 'a -> int) -> 'a array -> 'a array
val fast_sorted_copy        : ?compare:('a -> 'a -> int) -> 'a array -> 'a array
val sort_saving_positions   : ?compare:('a -> 'a -> int) -> 'a array -> (int * 'a) array
val sort_saving_permutation : ?compare:('a -> 'a -> int) -> 'a array -> (int array) * ('a array)
val apply_permutation       : int array -> 'a array -> 'a array 
val undo_permutation        : int array -> 'a array -> 'a array 

val for_all : (int -> 'a -> bool) -> 'a array -> bool
val exists  : (int -> 'a -> bool) -> 'a array -> bool
val lexists : (int -> 'a -> bool) -> 'a array -> int option
val rexists : (int -> 'a -> bool) -> 'a array -> int option

val search  : ('a -> bool) -> 'a array -> 'a option
val searchi : ('a -> bool) -> 'a array -> (int * 'a) option
val find    : ('a -> bool) -> 'a array -> 'a
val findi   : ('a -> bool) -> 'a array -> (int * 'a)

val search_longest_sequence : ?leftmost:unit -> ('a -> bool) -> 'a array -> (int * int) option
val shared_property : ('a -> 'b) -> 'a array -> bool

val random_permutation : 'a array -> 'a array
val frequence : ('a -> bool) -> 'a array -> int * float
val count     : ('a -> bool) -> 'a array -> int

val values_such_that             : (int -> 'a -> bool) -> 'a array -> 'a list
val indexes_such_that            : (int -> 'a -> bool) -> 'a array -> int list
val indexes_and_values_such_that : (int -> 'a -> bool) -> 'a array -> (int * 'a) list

(* The call {[dichotomic_search a x]} returns a pair (b,i) that
   provides two distinct kind of helpful informations:
   1) if b=true  then x has been found at position i
   2) if b=false then x has not been found and i contains
      the *first* element y strictly greater than x or i is
      out of bounds (i>=length, that means x greater than all
      elements in the array)
   *)
val dichotomic_search : ?a:int -> ?b:int -> 'a array -> 'a -> bool * int
val dichotomic_insert : 'a array -> 'a -> 'a array
val dichotomic_index_of_first_element_gt : ?a:int -> ?b:int -> 'a -> 'a array -> int option
val dichotomic_index_of_first_element_ge : ?a:int -> ?b:int -> 'a -> 'a array -> int option
val dichotomic_index_of_last_element_lt  : ?a:int -> ?b:int -> 'a -> 'a array -> int option
val dichotomic_index_of_last_element_le  : ?a:int -> ?b:int -> 'a -> 'a array -> int option

val for_all2 : (int -> 'a -> 'b -> bool) -> 'a array -> 'b array -> bool
val exists2  : (int -> 'a -> 'b -> bool) -> 'a array -> 'b array -> bool

val iter2  : ('a -> 'b -> unit) -> 'a array -> 'b array -> unit
val iteri2 : (int -> 'a -> 'b -> unit) -> 'a array -> 'b array -> unit

val map2   : ('a -> 'b -> 'c) -> 'a array -> 'b array -> 'c array
val mapi2  : (int -> 'a -> 'b -> 'c) -> 'a array -> 'b array -> 'c array

val map_fold : ('a -> int -> 'b -> 'c * 'a) -> 'a -> 'b array -> 'c array

val fold_lefti  : (int -> 'a -> 'b -> 'a) -> 'a -> 'b array -> 'a
val fold_righti : (int -> 'a -> 'b -> 'b) -> 'a array -> 'b -> 'b

val fold_left2 : ('a -> 'b -> 'c -> 'a) -> 'a -> 'b array -> 'c array -> 'a
val fold_right2 : ('a -> 'b -> 'c -> 'c) -> 'a array -> 'b array -> 'c -> 'c

val fold_lefti2 : (int -> 'a -> 'b -> 'c -> 'a) -> 'a -> 'b array -> 'c array -> 'a
val fold_righti2 : (int -> 'a -> 'b -> 'c -> 'c) -> 'a array -> 'b array -> 'c -> 'c

val fold_binop : ('a -> 'a -> 'a) -> 'a array -> 'a

val init2 : int -> (int -> 'a *'b) -> 'a array * 'b array
val split : ('a * 'b) array -> 'a array * 'b array
val combine : 'a array -> 'b array -> ('a * 'b) array

val cut : lengths:int list -> 'a array -> 'a array list

val max  : ?gt:('a -> 'a -> bool) -> 'a array -> int * 'a
val min  : ?gt:('a -> 'a -> bool) -> 'a array -> int * 'a
val best : ?choice:('a -> 'a -> 'a) -> 'a array -> int * 'a

module Matrix : sig
 type 'a t = 'a array array
 val init : int -> int -> (int -> int -> 'a) -> 'a t
 val of_list : 'a list list -> 'a t
 val to_list : 'a t -> 'a list list
 val transpose : 'a t -> 'a t
end
