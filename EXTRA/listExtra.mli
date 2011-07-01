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

(** Additional features for the standard module [List]. *)

val filter_map : ?acc:('b list) -> ('a -> 'b option) -> 'a list -> 'b list

val map      : ?acc:'b list -> ('a -> 'b) -> 'a list -> 'b list
val mapi     : ?acc:'a list -> (int -> 'b -> 'a) -> 'b list -> 'a list
val rev_map  : ?acc:'b list -> ('a -> 'b) -> 'a list -> 'b list
val rev_mapi : ?acc:'b list -> (int -> 'a -> 'b) -> 'a list -> 'b list

val flatten : ?acc:'a list -> 'a list list -> 'a list
val foreach      : 'a list -> ('a -> unit) -> unit

val combine3 : 'a list -> 'b list -> 'c list -> ('a * 'b * 'c) list
val combine4 : 'a list -> 'b list -> 'c list -> 'd list -> ('a * 'b * 'c * 'd) list

val iteri : (int -> 'a -> unit) -> 'a list -> unit

val shared_property : ('a->'b) -> 'a list -> bool

(** {2 Generalizations} *)

val head : ?n:int -> 'a list -> 'a list
val tail : ?i:int -> 'a list -> 'a list

val search  : ('a -> bool) -> 'a list -> 'a option
val searchi : ('a -> bool) -> 'a list -> (int * 'a) option
val findi   : ('a -> bool) -> 'a list -> (int * 'a)

(** {2 Set operations} *)

val substract    : 'a list -> 'a list -> 'a list
val subset       : 'a list -> 'a list -> bool
val eqset        : 'a list -> 'a list -> bool
val intersection : 'a list -> 'a list -> 'a list
val uniq         : 'a list -> 'a list
val remove_duplicates : ?take_first:bool -> 'a list -> 'a list

val product2 : 'a list -> 'b list -> ('a * 'b) list
val product3 : 'a list -> 'b list -> 'c list -> ('a * 'b * 'c) list

(** {2 Indexes} *)

val int_seq   : min:int   -> max:int   -> incr:int   -> int list
val float_seq : min:float -> max:float -> incr:float -> float list

val range      : int -> int -> int list
val interval   : int -> int -> int list
val indexes    : 'a list -> int list
val asFunction : int list -> int -> int

(** {b Selecting by indexes} *)

val select : 'a list -> int list -> 'a list

(** {b Removing by indexes} *)

val rmindex : 'a list -> int -> 'a list

(** {b Searching for indexes} *)

val indexSuchThat : ('a -> bool) -> 'a list -> int option
val indexOf       : 'a -> 'a list -> int option
val firstIndexOf  : 'a -> 'a list -> int option
val lastIndexOf   : 'a -> 'a list -> int option

(** {2 Permutations} *)

val shuffle : 'a list -> 'a list
val permute : (int -> int) -> 'a list -> 'a list
val shuffler : 'a list -> int -> int
val shuffleIndexes : 'a list -> int list
val lift_to_the_top_positions : ('a -> bool) -> 'a list -> 'a list

(** {2 Folding} *)

val big : ('a -> 'a -> 'a) -> 'a list -> 'a
val max : 'a list -> 'a
val min : 'a list -> 'a

(** {2 List of lists} *)

val transpose : 'a list list -> 'a list list

(** Association lists. Not more than 1 binding per key ensured. *)
module Assoc :
  sig
    val mem        : 'a -> ('a * 'b) list -> bool
    val remove     : 'a -> ('a * 'b) list -> ('a * 'b) list
    val find       : 'a -> ('a * 'b) list -> 'b
    val add        : 'a -> 'b -> ('a * 'b) list -> ('a * 'b) list
    val find_first : 'a list -> ('a * 'b) list -> 'b
  end

(** Association lists (with physical equality).
    Not more than 1 binding per key ensured. *)
module Assq :
  sig
    val mem        : 'a -> ('a * 'b) list -> bool
    val remove     : 'a -> ('a * 'b) list -> ('a * 'b) list
    val find       : 'a -> ('a * 'b) list -> 'b
    val add        : 'a -> 'b -> ('a * 'b) list -> ('a * 'b) list
    val find_first : 'a list -> ('a * 'b) list -> 'b
  end
