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

(** Additional features for the standard module [Pervasives]. *)

val round : ?decimals:int -> float -> float

val for_float : ?break:('a -> float -> bool) -> ?backward:unit -> min:float -> max:float -> step:float -> ('a -> float -> 'a) -> 'a -> 'a
val for_int   : ?break:('a -> int -> bool) -> ?backward:unit -> ?step:int -> min:int -> max:int -> ('a -> int -> 'a) -> 'a -> 'a
