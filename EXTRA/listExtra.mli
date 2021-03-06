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

type 'a t = 'a list

val filter_map : ?acc:('b list) -> ('a -> 'b option) -> 'a list -> 'b list
val filteri    : ?acc:'a list -> (int -> 'a -> bool) -> 'a list -> 'a list
val find_map   : ('a -> 'b option) -> 'a list -> 'b

val map      : ?acc:'b list -> ('a -> 'b) -> 'a list -> 'b list
val mapi     : ?acc:'a list -> (int -> 'b -> 'a) -> 'b list -> 'a list
val rev_map  : ?acc:'b list -> ('a -> 'b) -> 'a list -> 'b list
val rev_mapi : ?acc:'b list -> (int -> 'a -> 'b) -> 'a list -> 'b list

val fold_left_zipper : ('a -> 'b list * 'b * 'b list -> 'a) -> 'a -> 'b list -> 'a

val perm_fold : ?disorder:unit -> ('a -> 'b list -> 'a) -> 'a -> 'b list -> 'a
val perm_iter : ?disorder:unit -> ('a list -> unit) -> 'a list -> unit
val perm_map  : ?disorder:unit -> ('a list -> 'b) -> 'a list -> 'b list
val perm      : ?disorder:unit -> 'a list -> 'a list list

val comb_fold : ?disorder:unit -> k:int -> ('a -> 'b list -> 'a) -> 'a -> 'b list -> 'a
val comb_iter : ?disorder:unit -> k:int -> ('a list -> unit) -> 'a list -> unit
val comb_map  : ?disorder:unit -> k:int -> ('a list -> 'b) -> 'a list -> 'b list
val comb      : ?disorder:unit -> k:int -> 'b list -> 'b list list

val k_perm_fold : ?disorder:unit -> k:int -> ('a -> 'b list -> 'a) -> 'a -> 'b list -> 'a
val k_perm_iter : ?disorder:unit -> k:int -> ('a list -> unit) -> 'a list -> unit
val k_perm_map  : ?disorder:unit -> k:int -> ('a list -> 'b) -> 'a list -> 'b list
val k_perm      : ?disorder:unit -> k:int -> 'a list -> 'a list list

(*
val comb      : k:int -> 'b list -> 'b list list
val comb_fold : k:int -> ('a -> 'b list -> 'a) -> 'a -> 'b list -> 'a
val comb_iter : k:int -> ('a list -> unit) -> 'a list -> unit
val comb_map  : k:int -> ('a list -> 'b) -> 'a list -> 'b list
*)

(** Cartesian products: *)

val product2 : 'a list -> 'b list -> ('a * 'b) list
val product3 : 'a list -> 'b list -> 'c list -> ('a * 'b * 'c) list
val product4 : 'a list -> 'b list -> 'c list -> 'd list -> ('a * 'b * 'c * 'd) list
val product5 : 'a list -> 'b list -> 'c list -> 'd list -> 'e list -> ('a * 'b * 'c * 'd * 'e) list
val product6 : 'a list -> 'b list -> 'c list -> 'd list -> 'e list -> 'f list -> ('a * 'b * 'c * 'd * 'e * 'f) list
val product7 : 'a list -> 'b list -> 'c list -> 'd list -> 'e list -> 'f list -> 'g list -> ('a * 'b * 'c * 'd * 'e * 'f * 'g) list
val product8 : 'a list -> 'b list -> 'c list -> 'd list -> 'e list -> 'f list -> 'g list -> 'h list -> ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h) list

type 'a tuple = 'a list
val product : 'a list tuple -> 'a tuple list

val map_folding  : ?acc:'b list ->        ('s -> 'a -> 'b * 's) -> 's -> 'a list -> 'b list
val mapi_folding : ?acc:'b list -> (int -> 's -> 'a -> 'b * 's) -> 's -> 'a list -> 'b list
(* --- *)
val map_fold     : ?acc:'b list ->        ('s -> 'a -> 'b) ->        ('s -> 'a -> 's) -> 's -> 'a list -> 'b list * 's
val mapi_fold    : ?acc:'b list -> (int -> 's -> 'a -> 'b) -> (int -> 's -> 'a -> 's) -> 's -> 'a list -> 'b list * 's

val init : int -> (int -> 'a) -> 'a list

val flatten : ?acc:'a list -> 'a list list -> 'a list
val foreach      : 'a list -> ('a -> unit) -> unit

val combine3 : 'a list -> 'b list -> 'c list -> ('a * 'b * 'c) list
val combine4 : 'a list -> 'b list -> 'c list -> 'd list -> ('a * 'b * 'c * 'd) list
val combine5 : 'a list -> 'b list -> 'c list -> 'd list -> 'e list -> ('a * 'b * 'c * 'd * 'e) list
val combine6 : 'a list -> 'b list -> 'c list -> 'd list -> 'e list -> 'f list -> ('a * 'b * 'c * 'd * 'e * 'f) list

val combine7 : 'a list -> 'b list -> 'c list -> 'd list -> 'e list -> 'f list -> 'g list
  -> ('a * 'b * 'c * 'd * 'e * 'f * 'g) list

val combine8 : 'a list -> 'b list -> 'c list -> 'd list -> 'e list -> 'f list -> 'g list -> 'h list
  -> ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h) list

val split2 : ('a * 'b) list -> 'a list * 'b list
val split3 : ('a * 'b * 'c) list -> 'a list * 'b list * 'c list
val split4 : ('a * 'b * 'c * 'd) list -> 'a list * 'b list * 'c list * 'd list
val split5 : ('a * 'b * 'c * 'd * 'e) list -> 'a list * 'b list * 'c list * 'd list * 'e list
val split6 : ('a * 'b * 'c * 'd * 'e * 'f) list -> 'a list * 'b list * 'c list * 'd list * 'e list * 'f list

val split7 : ('a * 'b * 'c * 'd * 'e * 'f * 'g) list
  -> 'a list * 'b list * 'c list * 'd list * 'e list * 'f list * 'g list

val split8 : ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h) list
  -> 'a list * 'b list * 'c list * 'd list * 'e list * 'f list * 'g list * 'h list

val iteri : (int -> 'a -> unit) -> 'a list -> unit

val shared_property : ('a->'b) -> 'a list -> bool

val cut : lengths:int list -> 'a list -> 'a list list

(** {2 Generalizations} *)

val head : ?n:int -> 'a list -> 'a list
val tail : ?i:int -> 'a list -> 'a list

val search  : ('a -> bool) -> 'a list -> 'a option
val searchi : ('a -> bool) -> 'a list -> (int * 'a) option
val findi   : ('a -> bool) -> 'a list -> (int * 'a)

val first_success : ('a -> 'b option) -> 'a list -> 'b option

(** {2 Set operations} *)

val substract    : 'a list -> 'a list -> 'a list
val subset       : 'a list -> 'a list -> bool
val eqset        : 'a list -> 'a list -> bool
val intersection : 'a list -> 'a list -> 'a list
val uniq         : 'a list -> 'a list
val remove_duplicates : ?take_first:bool -> 'a list -> 'a list

val amass        : size:int -> 'a list -> 'a list list

(** {2 Indexes} *)

val int_seq   : min:int   -> max:int   -> incr:int   -> int list
val float_seq : min:float -> max:float -> incr:float -> float list

val range      : int -> int -> int list
val interval   : int -> int -> int list
val indexes    : 'a list -> int list
val asFunction : int list -> int -> int

(** {b Selecting by indexes} *)

val select : 'a list -> int list -> 'a list
val select_from_to : 'a list -> int -> int -> 'a list

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

val fold_binop  : ('a -> 'a -> 'a) -> 'a list -> 'a
val big         : ('a -> 'a -> 'a) -> 'a list -> 'a

val max  : 'a list -> 'a
val min  : 'a list -> 'a
val best : ?choice:('a -> 'a -> 'a) -> 'a list -> 'a

(** {2 Printing} *)

val  printf   : ?frame:(string -> string, unit, string) format -> ?sep:string -> ('a -> string, unit, string) format -> 'a list -> unit
val eprintf   : ?frame:(string -> string, unit, string) format -> ?sep:string -> ('a -> string, unit, string) format -> 'a list -> unit
val sprintf   : ?frame:(string -> string, unit, string) format -> ?sep:string -> ('a -> string, unit, string) format -> 'a list -> string
val to_string : ?frame:(string -> string, unit, string) format -> ?sep:string -> ('a -> string) -> 'a list -> string

(** {2 List of lists} *)

val transpose : 'a list list -> 'a list list

(** Association lists.
    Not more than 1 binding per key ensured. *)
module Assoc :
  sig
    val mem        : 'a -> ('a * 'b) list -> bool
    val remove     : 'a -> ('a * 'b) list -> ('a * 'b) list
    val find       : 'a -> ('a * 'b) list -> 'b
    val add        : 'a -> 'b -> ('a * 'b) list -> ('a * 'b) list
    val set        : ('a * 'b) list -> 'a -> 'b ->  ('a * 'b) list (* add with flipped arguments *)
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
    val set        : ('a * 'b) list -> 'a -> 'b ->  ('a * 'b) list (* add with flipped arguments *)
    val find_first : 'a list -> ('a * 'b) list -> 'b
  end
