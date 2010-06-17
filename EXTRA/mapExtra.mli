(* This file is part of ocamlbricks
   Copyright (C) 2010 Jean-Vincent Loddo

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

(** Additional features for (and instances of) the standard module [Map]. *)

module Extend :
  functor (M : Map.S) ->
    sig
      type key = M.key
      type 'a t
      val empty     : 'a t
      val is_empty  : 'a t -> bool
      val add       : key -> 'a -> 'a t -> 'a t
      val find      : key -> 'a t -> 'a
      val remove    : key -> 'a t -> 'a t
      val mem       : key -> 'a t -> bool
      val iter      : (key -> 'a -> unit) -> 'a t -> unit
      val map       : ('a -> 'b) -> 'a t -> 'b t
      val mapi      : (key -> 'a -> 'b) -> 'a t -> 'b t
      val fold      : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
      val compare   : ('a -> 'a -> int) -> 'a t -> 'a t -> int
      val equal     : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

      (* Extra functions: *)

      val filter    : (key -> 'a -> bool) -> 'a t -> 'a t
      val of_list   : ?acc:'a t -> (key * 'a) list -> 'a t
      val to_list   : ?acc:(key * 'a) list -> ?reverse:bool -> 'a t -> (key * 'a) list
      val domain    : ?reverse:bool -> 'a t -> key list
      val codomain  : ?reverse:bool -> 'a t -> 'a list
      val restrict  : 'a t -> key list -> 'a t
      val substract : 'a t -> key list -> 'a t
    end

module Make :
  functor (Ord : Map.OrderedType) ->
    sig
      type key = Ord.t
      type 'a t
      val empty     : 'a t
      val is_empty  : 'a t -> bool
      val add       : key -> 'a -> 'a t -> 'a t
      val find      : key -> 'a t -> 'a
      val remove    : key -> 'a t -> 'a t
      val mem       : key -> 'a t -> bool
      val iter      : (key -> 'a -> unit) -> 'a t -> unit
      val map       : ('a -> 'b) -> 'a t -> 'b t
      val mapi      : (key -> 'a -> 'b) -> 'a t -> 'b t
      val fold      : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
      val compare   : ('a -> 'a -> int) -> 'a t -> 'a t -> int
      val equal     : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

      (* Extra functions: *)
      
      val filter    : (key -> 'a -> bool) -> 'a t -> 'a t
      val of_list   : ?acc:'a t -> (key * 'a) list -> 'a t
      val to_list   : ?acc:(key * 'a) list -> ?reverse:bool -> 'a t -> (key * 'a) list
      val domain    : ?reverse:bool -> 'a t -> key list
      val codomain  : ?reverse:bool -> 'a t -> 'a list
      val restrict  : 'a t -> key list -> 'a t
      val substract : 'a t -> key list -> 'a t
    end


(** {2 Pre-builded mappings} *)

module String_map :
  sig
    type key = string
    type +'a t
    val empty     : 'a t
    val is_empty  : 'a t -> bool
    val add       : string -> 'a -> 'a t -> 'a t
    val find      : string -> 'a t -> 'a
    val remove    : string -> 'a t -> 'a t
    val mem       : string -> 'a t -> bool
    val iter      : (string -> 'a -> unit) -> 'a t -> unit
    val map       : ('a -> 'b) -> 'a t -> 'b t
    val mapi      : (string -> 'a -> 'b) -> 'a t -> 'b t
    val fold      : (string -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val compare   : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal     : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

    val filter    : (string -> 'a -> bool) -> 'a t -> 'a t
    val of_list   : ?acc:'a t -> (string * 'a) list -> 'a t
    val to_list   : ?acc:(string * 'a) list -> ?reverse:bool -> 'a t -> (string * 'a) list
    val domain    : ?reverse:bool -> 'a t -> string list
    val codomain  : ?reverse:bool -> 'a t -> 'a list
    val restrict  : 'a t -> string list -> 'a t
    val substract : 'a t -> string list -> 'a t
  end
  
module Int_map :
  sig
    type key = int
    type +'a t
    val empty     : 'a t
    val is_empty  : 'a t -> bool
    val add       : int -> 'a -> 'a t -> 'a t
    val find      : int -> 'a t -> 'a
    val remove    : int -> 'a t -> 'a t
    val mem       : int -> 'a t -> bool
    val iter      : (int -> 'a -> unit) -> 'a t -> unit
    val map       : ('a -> 'b) -> 'a t -> 'b t
    val mapi      : (int -> 'a -> 'b) -> 'a t -> 'b t
    val fold      : (int -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val compare   : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal     : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

    val filter    : (int -> 'a -> bool) -> 'a t -> 'a t
    val of_list   : ?acc:'a t -> (int * 'a) list -> 'a t
    val to_list   : ?acc:(int * 'a) list -> ?reverse:bool -> 'a t -> (int * 'a) list
    val domain    : ?reverse:bool -> 'a t -> int list
    val codomain  : ?reverse:bool -> 'a t -> 'a list
    val restrict  : 'a t -> int list -> 'a t
    val substract : 'a t -> int list -> 'a t
  end

(** {2 Not persistent (imperative) versions} *)

module Destructive : sig

  module Make :
  functor (Ord : Map.OrderedType) ->
    sig
      type key = Ord.t
      type 'a t
      val create    : unit -> 'a t
      val is_empty  : 'a t -> bool
      val add       : key -> 'a -> 'a t -> unit
      val find      : key -> 'a t -> 'a
      val remove    : key -> 'a t -> unit
      val mem       : key -> 'a t -> bool
      val iter      : (key -> 'a -> unit) -> 'a t -> unit
      val map       : ('a -> 'a) -> 'a t -> unit
      val mapi      : (key -> 'a -> 'a) -> 'a t -> unit
      val fold      : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
      val compare   : ('a -> 'a -> int) -> 'a t -> 'a t -> int
      val equal     : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

      (* Extra functions: *)

      val copy      : 'a t -> 'a t
      val filter    : (key -> 'a -> bool) -> 'a t -> unit
      val of_list   : ?acc:'a t -> (key * 'a) list -> 'a t
      val to_list   : ?acc:(key * 'a) list -> ?reverse:bool -> 'a t -> (key * 'a) list
      val domain    : ?reverse:bool -> 'a t -> key list
      val codomain  : ?reverse:bool -> 'a t -> 'a list
      val restrict  : 'a t -> key list -> unit
      val substract : 'a t -> key list -> unit
    end

 (* Destructive version: *)
 module String_map :
  sig
    type key = string
    type 'a t
    val create : unit -> 'a t
    val is_empty : 'a t -> bool
    val add : string -> 'a -> 'a t -> unit
    val find : string -> 'a t -> 'a
    val remove : string -> 'a t -> unit
    val mem : string -> 'a t -> bool
    val iter : (string -> 'a -> unit) -> 'a t -> unit
    val map : ('a -> 'a) -> 'a t -> unit
    val mapi : (string -> 'a -> 'a) -> 'a t -> unit
    val fold : (string -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val copy : 'a t -> 'a t
    val filter : (string -> 'a -> bool) -> 'a t -> unit
    val of_list : ?acc:'a t -> (string * 'a) list -> 'a t
    val to_list :
      ?acc:(string * 'a) list -> ?reverse:bool -> 'a t -> (string * 'a) list
    val domain : ?reverse:bool -> 'a t -> string list
    val codomain : ?reverse:bool -> 'a t -> 'a list
    val restrict : 'a t -> string list -> unit
    val substract : 'a t -> string list -> unit
  end

 (* Destructive version: *)
  module Int_map :
  sig
    type key = int
    type 'a t
    val create : unit -> 'a t
    val is_empty : 'a t -> bool
    val add : int -> 'a -> 'a t -> unit
    val find : int -> 'a t -> 'a
    val remove : int -> 'a t -> unit
    val mem : int -> 'a t -> bool
    val iter : (int -> 'a -> unit) -> 'a t -> unit
    val map : ('a -> 'a) -> 'a t -> unit
    val mapi : (int -> 'a -> 'a) -> 'a t -> unit
    val fold : (int -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val copy : 'a t -> 'a t
    val filter : (int -> 'a -> bool) -> 'a t -> unit
    val of_list : ?acc:'a t -> (int * 'a) list -> 'a t
    val to_list :
      ?acc:(int * 'a) list -> ?reverse:bool -> 'a t -> (int * 'a) list
    val domain : ?reverse:bool -> 'a t -> int list
    val codomain : ?reverse:bool -> 'a t -> 'a list
    val restrict : 'a t -> int list -> unit
    val substract : 'a t -> int list -> unit
  end                                                                                                                                                                                              
end (* Destructive *)