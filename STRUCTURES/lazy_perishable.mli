(* This file is part of ocamlbricks
   Copyright (C) 2015  Jean-Vincent Loddo

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

(* Do not remove the following comment: it's an ocamldoc workaround. *)
(** *)

(** Lazy values with a lifetime. When the delay is expired, the value is recalculated. *)
 
type 'a t = 'a Thunk.t

type seconds = float
type lifetime = seconds 

val create : 'a Thunk.t -> lifetime -> 'a t
val force  : 'a t -> 'a  

