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

(** Additional features for the standard module [Filename]. *)

val add_extension_if_absent : string -> string -> string
val get_extension : ?with_dot:unit -> string -> string option
val get_extension_or_default : ?with_dot:unit -> ?default:string -> string -> string

val concat_list : string list -> string

val temp_dir :
  ?temp_dir:string ->
  ?prefix:string ->
  ?suffix:string ->
  ?perm:int ->
  unit -> string
  