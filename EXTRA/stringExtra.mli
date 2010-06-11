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

(** Additional features for the standard module [String]. *)

(** {2 Importing & copying} *)

type blit_function = string -> int -> string -> int -> int -> unit
val blitting : perform:(char -> int -> unit) -> blit_function

val from_descr   : ?blit:blit_function -> Unix.file_descr -> string
val from_file    : ?blit:blit_function -> string -> string
val from_channel : ?blit:blit_function -> in_channel -> string
val from_string  : perform:(char -> int -> unit) -> string -> string

(** {2 Searching indexes} *)

val nth_index_from  : string -> int -> char -> int -> int
val nth_rindex_from : string -> int -> char -> int -> int
val nth_index       : string -> char -> int -> int
val nth_rindex      : string -> char -> int -> int
val for_all         : (char -> bool) -> string -> bool
val for_all_i       : (int -> char -> bool) -> string -> bool
val exists          : (char -> bool) -> string -> bool
val exists_i        : (int -> char -> bool) -> string -> bool
val lexists         : (char -> bool) -> string -> int option
val rexists         : (char -> bool) -> string -> int option

(** {2 Relations} *)

val is_prefix       : string -> string -> bool

(** {2 Extracting sub-strings} *)

val tail          : string -> int -> string
val head          : string -> int -> string
val frame         : string -> char -> int -> int -> string
val frame_strict  : string -> char -> int -> int -> string
val rframe        : string -> char -> int -> int -> string
val rframe_strict : string -> char -> int -> int -> string

(** {2 Counting} *)

val count                      : string -> char -> int
val count_and_last_index       : string -> char -> int * int
val count_and_last_two_indexes : string -> char -> int * int * int

(** {2 Stripping} *)

val not_blank : char -> bool
val lstrip    : string -> string
val rstrip    : string -> string
val strip     : string -> string
val chop      : string -> string

(** {2 Splitting to char list} *)

val to_charlist : string -> char list
val of_charlist : char list -> string
val expand : (char -> string option) -> string -> string

module Charlist :
  sig
    val assemble              : char list -> string
    val disassemble_reversing : ?acc:char list -> string -> char list
    val assemble_reversing    : ?length:int -> char list -> string
  end

(** {2 Splitting to string list} *)

val cut   : ?n:int -> string -> string list
val split : ?squeeze:bool -> ?d:char -> string -> string list
val split_squeezing_blanks : ?blanks:char list -> string -> string list

(** {2 Merging strings} *)

val concat : ?blit:blit_function -> string list -> string
val merge  : string -> string -> string -> string
val quote  : ?l:string -> ?r:string -> string -> string
val assemble : string -> string -> string -> string

type binop = string -> string -> string
val big : binop -> string list -> string
val merge_map : ?sep:string -> ('a -> string) -> 'a list -> string

module Fold :
  sig
    val commacat         : string list -> string
    val semicolon        : string list -> string
    val nospacecommacat  : string list -> string
    val nospacesemicolon : string list -> string
    val dotcat           : string list -> string
    val newlinecat       : string list -> string
    val blankcat         : string list -> string
    val slashcat         : string list -> string
  end
val merge_fields : string -> int list -> string list -> string

(** {2 Text} *)

type line = string
val to_line : line -> line
module Text :
  sig
    type t = string list
    type filter = string list -> string list
    val to_string : line list -> line
    val of_string : ?squeeze:bool -> string -> string list
    module Matrix :
      sig
        type t = string list list
        type filter = t -> t
        val of_string : ?squeeze:bool -> ?d:char -> string -> string list list
        val to_string : ?d:string -> line list list -> line
      end
  end
