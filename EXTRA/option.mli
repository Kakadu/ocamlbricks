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
val extract_from_list : ?acc:'a list -> 'a option list -> 'a list

val map  : ('a -> 'b) -> 'a option -> 'b option
val bind : 'a option -> ('a -> 'b option) -> 'b option
val return : 'a -> 'a option

val iter : ('a -> unit) -> 'a option -> unit

val of_fallible_application : ?fallback:(exn -> 'a -> unit) -> ('a -> 'b) -> 'a -> 'b option

val of_bool : bool -> unit option
val to_bool : 'a option -> bool

val to_list : 'a option -> 'a list

(** {2 Printers}

The following functions act on a ['a option] value if the user is able to provide a function converting
the encapsulated value ['a] into a string.

{b Examples}:
{[
# printf string_of_float "Value is: %s\n" (Some 3.14) ;;
Value is: Some 3.14
  : unit = ()

# printf string_of_float "Value is: %s\n" None ;;
Value is: None
  : unit = ()
]} *)

val string_of         : ('a->string) -> 'a option -> string
val print             : ('a->string) -> 'a option -> unit
val prerr             : ('a->string) -> 'a option -> unit

val print_endline     : ('a->string) -> 'a option -> unit
val prerr_endline     : ('a->string) -> 'a option -> unit

val fprintf           : ('a -> string) -> out_channel -> (string -> 'b, out_channel, unit) format -> 'a option -> 'b
val eprintf           : ('a -> string) -> (string -> 'b, out_channel, unit) format -> 'a option -> 'b
val printf            : ('a -> string) -> (string -> 'b, out_channel, unit) format -> 'a option -> 'b
val sprintf           : ('a -> string) -> (string -> 'b, unit, string) format -> 'a option -> 'b
