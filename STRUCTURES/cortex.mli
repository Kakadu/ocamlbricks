(* This file is part of ocamlbricks
   Copyright (C) 2013  Jean-Vincent Loddo

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


type 'a t

val return :
  ?equality:('a -> 'a -> bool) ->
  ?on_proposal:('a -> 'a -> 'a) ->
  'a -> 'a t

val of_object :
  ?equality:('a -> 'a -> bool) ->
  ?on_proposal:('a -> 'a -> 'a) ->
  < get : 'a; set : 'a -> unit > -> 'a t

val on_proposal_append : 'a t -> ('a -> 'a -> 'a) -> Thunk.id
val on_proposal_remove : 'a t -> Thunk.id -> unit
val on_proposal_clear  : 'a t -> unit

val eval :
  ?guard:('a -> bool) ->
  ('a -> 'b -> 'a * ('a -> 'c)) ->
  'b ->
  'a t -> 'c * bool

(** Facilities (specific and common `eval' instances): *)
val get     : ?guard:('a -> bool) -> 'a t -> 'a
val set     : ?guard:('a -> bool) -> 'a t -> 'a -> unit
val propose : ?guard:('a -> bool) -> 'a t -> 'a -> 'a * bool
val move    : ?guard:('a -> bool) -> 'a t -> ('a -> 'a) -> 'a * bool
val apply   : ?guard:('a -> bool) -> 'a t -> ('a -> 'b -> 'c) -> 'b -> 'c

module Async : sig
  val set  : ?guard:('a -> bool) -> 'a t -> 'a -> unit
  val move : ?guard:('a -> bool) -> 'a t -> ('a -> 'a) -> unit
end

val group_single :
  ?on_proposal:('a -> 'a -> 'a) ->
  'a t -> 'a t

val group_pair :
  ?unprotect:unit ->
  ?on_proposal:('a * 'b -> 'a * 'b -> 'a * 'b) ->
  'a t ->
  'b t -> ('a * 'b) t

val group_triple :
  ?on_proposal:('a * 'b * 'c -> 'a * 'b * 'c -> 'a * 'b * 'c) ->
  'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t

val group_quadruple :
  ?on_proposal:('a * 'b * 'c * 'd -> 'a * 'b * 'c * 'd -> 'a * 'b * 'c * 'd) ->
  'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t

val group_quintuple :
  ?on_proposal:('a * 'b * 'c * 'd * 'e -> 'a * 'b * 'c * 'd * 'e -> 'a * 'b * 'c * 'd * 'e) ->
  'a t -> 'b t -> 'c t -> 'd t -> 'e t -> ('a * 'b * 'c * 'd * 'e) t

val group_array :
  ?on_proposal:('a array -> 'a array -> 'a array) ->
  ('a t) array -> ('a array) t

val ungroup :
 ?unprotect:unit ->
 'a t -> unit

val revno_equality : 'a t -> 'a t -> bool
val revno_or_content_equality : 'a t -> 'a t -> bool

(* Conversion to objects: *)

class type ['a] public_interface = object
  method eval    : 'b 'c. ?guard:('a -> bool) -> ('a -> 'b -> 'a * ('a -> 'c)) -> 'b -> 'c * bool
  method get     : ?guard:('a -> bool) -> unit -> 'a
  method set     : ?guard:('a -> bool) -> 'a -> unit
  method propose : ?guard:('a -> bool) -> 'a -> 'a * bool
  method move    : ?guard:('a -> bool) -> ('a -> 'a) -> 'a * bool
  method async   : <
    set  : ?guard:('a -> bool) -> 'a -> unit;
    move : ?guard:('a -> bool) -> ('a -> 'a) -> unit;
    >
end

class type ['a] private_interface = object
  method private eval    : 'b 'c. ?guard:('a -> bool) -> ('a -> 'b -> 'a * ('a -> 'c)) -> 'b -> 'c * bool
  method private get     : ?guard:('a -> bool) -> unit -> 'a
  method private set     : ?guard:('a -> bool) -> 'a -> unit
  method private propose : ?guard:('a -> bool) -> 'a -> 'a * bool
  method private move    : ?guard:('a -> bool) -> ('a -> 'a) -> 'a * bool
  method private async   : <
    set  : ?guard:('a -> bool) -> 'a -> unit;
    move : ?guard:('a -> bool) -> ('a -> 'a) -> unit;
    >
end

class ['a] to_object_with_public_interface  : 'a t -> ['a] public_interface
class ['a] to_object_with_private_interface : 'a t -> ['a] private_interface

val to_object_with_public_interface  : 'a t -> 'a public_interface
val to_object_with_private_interface : 'a t -> 'a private_interface

IFDEF DOCUMENTATION_OR_DEBUGGING THEN
module Example : sig val x : int t val y : int t val z : (int * int) t end
ENDIF

