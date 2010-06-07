(* This file is part of our reusable OCaml BRICKS library
   Copyright (C) 2010  Jean-Vincent Loddo

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

(** Additional features for (and instances of) the standard module [Set]. *)

module Extend :
  functor (S : Set.S) ->
    sig
      type elt = S.elt
      type t = S.t
      val empty : t
      val is_empty : t -> bool
      val mem : elt -> t -> bool
      val add : elt -> t -> t
      val singleton : elt -> t
      val remove : elt -> t -> t
      val union : t -> t -> t
      val inter : t -> t -> t
      val diff : t -> t -> t
      val compare : t -> t -> int
      val equal : t -> t -> bool
      val subset : t -> t -> bool
      val iter : (elt -> unit) -> t -> unit
      val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
      val for_all : (elt -> bool) -> t -> bool
      val exists : (elt -> bool) -> t -> bool
      val filter : (elt -> bool) -> t -> t
      val partition : (elt -> bool) -> t -> t * t
      val cardinal : t -> int
      val elements : t -> elt list
      val min_elt : t -> elt
      val max_elt : t -> elt
      val choose : t -> elt
      val split : elt -> t -> t * bool * t
      val copy : t -> t
      val of_list : ?acc:t -> elt list -> t
      val to_list : ?acc:elt list -> ?reverse:bool -> t -> elt list
      val uniq : elt list -> elt list
    end

module Make :
  functor (Ord : Set.OrderedType) ->
    sig
      type elt = Ord.t
      type t
      val empty : t
      val is_empty : t -> bool
      val mem : elt -> t -> bool
      val add : elt -> t -> t
      val singleton : elt -> t
      val remove : elt -> t -> t
      val union : t -> t -> t
      val inter : t -> t -> t
      val diff : t -> t -> t
      val compare : t -> t -> int
      val equal : t -> t -> bool
      val subset : t -> t -> bool
      val iter : (elt -> unit) -> t -> unit
      val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
      val for_all : (elt -> bool) -> t -> bool
      val exists : (elt -> bool) -> t -> bool
      val filter : (elt -> bool) -> t -> t
      val partition : (elt -> bool) -> t -> t * t
      val cardinal : t -> int
      val elements : t -> elt list
      val min_elt : t -> elt
      val max_elt : t -> elt
      val choose : t -> elt
      val split : elt -> t -> t * bool * t
      val copy : t -> t
      val of_list : ?acc:t -> elt list -> t
      val to_list : ?acc:elt list -> ?reverse:bool -> t -> elt list
      val uniq : elt list -> elt list
    end


module String_set :
  sig
    type t
    val empty : t
    val is_empty : t -> bool
    val mem : string -> t -> bool
    val add : string -> t -> t
    val singleton : string -> t
    val remove : string -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (string -> unit) -> t -> unit
    val fold : (string -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (string -> bool) -> t -> bool
    val exists : (string -> bool) -> t -> bool
    val filter : (string -> bool) -> t -> t
    val partition : (string -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> string list
    val min_elt : t -> string
    val max_elt : t -> string
    val choose : t -> string
    val split : string -> t -> t * bool * t
    val copy : t -> t
    val of_list : ?acc:t -> string list -> t
    val to_list : ?acc:string list -> ?reverse:bool -> t -> string list
    val uniq : string list -> string list
  end

module Int_set :
  sig
    type t
    val empty : t
    val is_empty : t -> bool
    val mem : int -> t -> bool
    val add : int -> t -> t
    val singleton : int -> t
    val remove : int -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (int -> unit) -> t -> unit
    val fold : (int -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (int -> bool) -> t -> bool
    val exists : (int -> bool) -> t -> bool
    val filter : (int -> bool) -> t -> t
    val partition : (int -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> int list
    val min_elt : t -> int
    val max_elt : t -> int
    val choose : t -> int
    val split : int -> t -> t * bool * t
    val copy : t -> t
    val of_list : ?acc:t -> int list -> t
    val to_list : ?acc:int list -> ?reverse:bool -> t -> int list
    val uniq : int list -> int list
  end
