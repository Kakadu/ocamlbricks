(* This file is part of ocamlbricks
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


val create : ?killable:unit -> ('a -> 'b) -> 'a -> Thread.t
val fork   : ?killable:unit -> ?behaviour:(pid:int->unit) -> ('a -> 'b) -> 'a -> Thread.t

val tutor  : ?killable:unit -> ?behaviour:(pid:int->unit) -> unit -> pid:int -> Thread.t

val at_exit : (unit -> unit) -> unit

val kill      : Thread.t -> bool
val killall   : unit -> unit
val killable  : unit -> int list
val killer    : Thread.t -> unit -> unit

val set_killable_with_thunk : ?who:Thread.t -> (unit -> unit) -> unit

val id_kill   : int -> bool
val id_killer : int -> unit -> unit

val delayed_kill    : float -> Thread.t -> unit
val delayed_killall : float -> unit
val delayed_id_kill : float -> int -> unit

val delay : float -> unit