(* This file is part of our reusable OCaml BRICKS library
   Copyright (C) 2009  Jean-Vincent Loddo

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

(** Replacement for the standard module [Stack]. The unique difference with the standard [Stack]
    is the function [to_list] that transforms the stack in a list in O(1). *)

type 'a t
exception Empty
val create   : unit -> 'a t
val clear    : 'a t -> unit
val copy     : 'a t -> 'a t
val push     : 'a -> 'a t -> unit
val pop      : 'a t -> 'a
val top      : 'a t -> 'a
val is_empty : 'a t -> bool
val length   : 'a t -> int
val iter     : ('a -> unit) -> 'a t -> unit
val to_list  : 'a t -> 'a list
