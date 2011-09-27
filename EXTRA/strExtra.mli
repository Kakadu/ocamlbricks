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

(** Additional features for the standard library [Str]. *)

val mkregexp : ?strict:bool -> string list -> string list -> string list -> Str.regexp

(** {2 High-level matching} *)

type result = (int * string * string list * int) option

val match_whole    : Str.regexp -> string -> result
val match_string   : string -> string -> result
val match_frame    : Str.regexp -> string -> int * int -> result

(** {b Boolean versions} *)

module Bool :
  sig
    val match_whole : Str.regexp -> string -> bool
    val match_string : string -> string -> bool
    val match_frame : Str.regexp -> string -> int * int -> bool
  end

(** {2 Extract groups} *)

val matched_groups : int -> string -> string list
val extract_groups : Str.regexp -> string -> string list

(** {2 Tools} *)

val minus          : string -> string -> string
val grep           : ?before:int -> ?after:int -> string -> string list -> string list
val wellFormedName : ?allow_dash:bool -> string -> bool
