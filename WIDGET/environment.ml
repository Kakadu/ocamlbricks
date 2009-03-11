(* This file is part of our reusable OCaml BRICKS library
   Copyright (C) 2007  Jean-Vincent Loddo, Luca Saiu

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

open Sugar;;
open ListExtra;;
open StringExtra;;
open UnixExtra;;

open Hashmap;;

(* **************************************** *
              Class Env
 * **************************************** *)

(** {2 Environments }
    Environments are especially useful for maintaining the {e state}, intendend as a set
    of bindings, of a user interaction with a GUI. *)

(** The class of environments. An ('a,'b) environment is a set of
    <key,value> pairs, where key is of type 'a and value of type 'b.
    A single environment can't contain more than a single binding for
    each key. *)
class ['a,'b] env = fun () -> object (self)
  (** The internal representation of an environment. *)
  val table : ('a, 'b) Hashmap.t = Hashmap.make ()

  (** Accessors, transparently converting to/from alists: *)
  method to_list     = Hashmap.to_list table
  method add_list xs = Hashmap.add_list table xs

  (** High level accessors. *)
  (** Get the value associated to the given id (key). *)
  method get id    = Hashmap.lookup table id
  (** Add a pair (identifier,value) to the environment. *)
  method add (id,v) = Hashmap.add table id v
  (** Update the environment (self) by another environment which will "cover" previous links. @return self.*)
  method updatedBy (e:(('a,'b) env)) : (('a,'b) env) = List.iter (self#add) (e#to_list); (self :> (('a,'b) env))
end;;

(** Simple constructor for environments.*)
let make (l:('a*'b) list) = let e=(new env ()) in (e#add_list l); e;;


(* **************************************** *
          Class string_env
 * **************************************** *)

(** {2 String environments }
    The special (and quite common) case where keys are strings allows to better trace get failures. *)

exception Undefined_identifier of string
class ['b] string_env () = object
  inherit [string,'b] env () as super
  method get id = try (super#get id) with Not_found -> raise (Undefined_identifier id)
end;;

(** Simple constructor for string environments.*)
let make_string_env (l:(string*'b) list) = let e=(new string_env ()) in (e#add_list l); e;;


