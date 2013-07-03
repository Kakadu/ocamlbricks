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

(** Operations on type ['a option]. *)

(** Extract the encapsulated value. If the argument is [None], the optional [?fallback] is called.
    By default [fallback] is set to [fun ()->failwith "Option.extract"].*)
val extract : ?failwith_msg:string -> ?fallback:(unit -> 'a) -> 'a option -> 'a
val extract_or : 'a option -> 'a -> 'a
val extract_or_force : 'a option -> 'a Lazy.t -> 'a
val extract_from_list : ?acc:'a list -> 'a option list -> 'a list

val map    : ('a -> 'b) -> 'a option -> 'b option
val bind   : 'a option -> ('a -> 'b option) -> 'b option
val return : 'a -> 'a option
val map2   : ('a -> 'b -> 'c) -> 'a option -> 'b option -> 'c option
val bind2  : 'a option -> 'b option -> ('a -> 'b -> 'c option) -> 'c option
val join   : 'a option option -> 'a option

val iter : ('a -> unit) -> 'a option -> unit
val filter : ('a -> bool) -> 'a option -> 'a option

val apply_or_catch : ?fallback:(exn -> 'a -> unit) -> ('a -> 'b) -> 'a -> 'b option

val of_bool : bool -> unit option
val to_bool : 'a option -> bool

val to_list : 'a option -> 'a list
val to_string : ?a:('a -> string) -> 'a option -> string
