(* This file is part of our reusable OCaml BRICKS library
   Copyright (C) 2012  Jean-Vincent Loddo

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

(** Operations on type [('a,'b) Either.t]. *)

type ('a,'b) t = Left of 'a | Right of 'b
type ('a,'b) either = ('a,'b) t

(** Extract the encapsulated value. If the argument is [Left a], the optional [?fallback] is called on the value [a]. By default [fallback] is set to
[fun _ -> failwith "Either.extract"].*)
val extract : ?failwith_msg:string -> ?fallback:('a -> 'b) -> ('a,'b) t -> 'b
val extract_or        : ('a,'b) t -> 'b -> 'b
val extract_or_force  : ('a,'b) t-> 'b Lazy.t -> 'b
val extract_from_list : ?acc:'b list -> ('a,'b) t list -> 'b list

val iter : ('b -> unit) -> ('a,'b) t -> unit
val map  : ('b -> 'c) -> ('a,'b) t -> ('a,'c) t
val bind : ('a,'b) t -> ('b -> ('a,'b) t) -> ('a,'b) t
val return : 'b -> ('a,'b) t

val apply_or_catch : ('a -> 'b) -> 'a -> (exn, 'b) t

val of_bool : bool -> (unit, unit) t
val to_bool : ('a,'b) t -> bool

val list_of : ('a,'b) t -> 'b list

val to_string : ?a:('a -> string) -> ?b:('b -> string) -> ('a, 'b) t -> string

module Make_printers_for :
  functor (Alpha : sig type a val string_of : a -> string end) ->
  functor (Beta  : sig type b val string_of : b -> string end) ->
    sig
      type t = (Alpha.a, Beta.b) either
      val string_of     : t -> string
      val print         : t -> unit
      val prerr         : t -> unit
      val print_endline : t -> unit
      val prerr_endline : t -> unit
      val fprintf : out_channel -> (string -> 'a, out_channel, unit) format -> t -> 'a
      val eprintf : (string -> 'a, out_channel, unit) format -> t -> 'a
      val printf  : (string -> 'a, out_channel, unit) format -> t -> 'a
      val sprintf : (string -> 'a, unit, string) format      -> t -> 'a
    end

