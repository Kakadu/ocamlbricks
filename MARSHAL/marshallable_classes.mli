(* This file is part of ocamlbricks
   Copyright (C) 2012 Jean-Vincent Loddo

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


module Marshalling_env   : sig type t end
module Unmarshalling_env : sig type t end

type field_name = string
type object_structure

class marshallable_class :
  ?name:string ->
  marshaller:marshaller option ref ->
  unit ->
  object
    method marshaller : marshaller
  end

and marshaller :
  ?parent_class_name:string ->
  parent:marshallable_class ->
  unit ->
  object
    method save_to_string : string
    method save_to_file   : string -> unit

    method load_from_string : ?mapping:(field_name -> field_name) -> string -> unit
    method load_from_file   : ?mapping:(field_name -> field_name) -> string -> unit

    method compare : < marshaller : marshaller; .. > -> int
    method equals  : < marshaller : marshaller; .. > -> bool
    method md5sum  : string
    method hash    : int

    (* Reload the object with himself in order to make its components as possible simplest
       (remove the surplus of components attributes caused by some casting operations (:>)) *)
    method remake_simplest : unit

    method register_simple_field :
      ?name:string ->                                          (* the field name *)
      (unit -> 'a) ->                                          (* the field getter *)
      ('a -> unit) ->                                          (* the field setter *)
      unit

    method register_object_field :
      ?name:string ->                                          (* the field name *)
      (unit -> (< marshaller : marshaller; .. > as 'obj)) ->   (* the field getter *)
      ('obj -> unit) ->                                        (* the field setter *)
      unit

    method register_functorized_object_field :
    'obj 'obj_t 'a 'b 'a_t 'b_t.
      ?name:string ->                                          (* the field name *)
      (('a -> 'b) -> 'a_t -> 'b_t) ->                          (* the functor *)
      (unit -> (< marshaller : marshaller; .. > as 'obj)) ->   (* the object maker (simplest constructor) *)
      (unit -> 'obj_t) ->                                      (* the field getter *)
      ('obj_t -> unit) ->                                      (* the field setter *)
      unit

    method register_bifunctorized_objects_field :
    'obj1 'obj2 'objects_t 'a 'b 'c 'd 'ac_t 'bd_t.
      ?name:string ->                                          (* name *)
      (('a -> 'b) -> ('c -> 'd) -> 'ac_t -> 'bd_t) ->          (* bifunctor *)
      (unit -> (< marshaller : marshaller; .. > as 'obj1)) ->  (* object maker 1 (simplest constructor) *)
      (unit -> (< marshaller : marshaller; .. > as 'obj2)) ->  (* object maker 2 (simplest constructor) *)
      (unit -> 'objects_t) ->                                  (* getter *)
      ('objects_t -> unit) ->                                  (* setter *)
        unit

    method register_trifunctorized_objects_field :
    'obj1 'obj2 'obj3 'objects_t 'a 'b 'c 'd 'e 'f 'ace_t 'bdf_t .
      ?name:string ->                                          (* name *)
      (('a -> 'b) -> ('c -> 'd) -> ('e -> 'f) -> 'ace_t -> 'bdf_t) -> (* trifunctor *)
      (unit -> (< marshaller : marshaller; .. > as 'obj1)) ->  (* object maker 1 (simplest constructor) *)
      (unit -> (< marshaller : marshaller; .. > as 'obj2)) ->  (* object maker 2 (simplest constructor) *)
      (unit -> (< marshaller : marshaller; .. > as 'obj3)) ->  (* object maker 3 (simplest constructor) *)
      (unit -> 'objects_t) ->                                  (* getter *)
      ('objects_t -> unit) ->                                  (* setter *)
        unit

    method parent_class_name : string option

    (* Internal methods (shoud be protected), not for users: *)
    method protected_load_from_object_structure : Unmarshalling_env.t -> object_structure -> unit * Unmarshalling_env.t
    method protected_save_to_object_structure   : Marshalling_env.t -> object_structure * Marshalling_env.t

  end


val marshallable_classes_version  : string
val marshallable_classes_metadata : unit -> string

val enable_warnings  : unit -> unit
val disable_warnings : unit -> unit

val enable_tracing  : unit -> unit
val disable_tracing : unit -> unit

(* After class definition, don't forget to register a basic constructor: *)
(*type 'a whatever_object = < .. > as 'a
type 'a basic_class_constructor = unit -> 'a whatever_object
val register_basic_constructor : class_name:string -> 'a basic_class_constructor -> unit
val get_basic_constructor : string -> involved_field:string -> involved_class:string -> 'a basic_class_constructor*)

IFDEF DOCUMENTATION_OR_DEBUGGING THEN
module Example :
  sig
    class class1 :
      ?marshaller:marshaller option ref ->
      unit ->
      object
        method get_field0 : int
        method set_field0 : int -> unit

        method get_field1 : string
        method set_field1 : string -> unit

        method get_field2 : int option
        method set_field2 : int option -> unit

        method marshaller : marshaller
      end
    class class2 :
      ?marshaller:marshaller option ref ->
      unit ->
      object
        method get_field0 : int
        method set_field0 : int -> unit

        method get_field1 : string
        method set_field1 : string -> unit

        method get_field2 : int option
        method set_field2 : int option -> unit

        method get_field3 : class1
        method set_field3 : class1 -> unit

        method get_field4 : class2 option
        method set_field4 : class2 option -> unit

        method get_field5 : class2 list
        method set_field5 : class2 list -> unit

        method marshaller : marshaller
      end
    class class3 :
      ?marshaller:marshaller option ref ->
      unit ->
      object
        method get_field0 : int
        method set_field0 : int -> unit

        method get_field1 : string
        method set_field1 : string -> unit

        method get_field2 : int option
        method set_field2 : int option -> unit

        method get_field3 : class1
        method set_field3 : class1 -> unit

        method get_field4 : class2 option
        method set_field4 : class2 option -> unit

        method get_field5 : class2 list
        method set_field5 : class2 list -> unit

        method get_field6 : char
        method set_field6 : char -> unit

        method get_field7 : (class2, class3) Either.t
        method set_field7 : (class2, class3) Either.t -> unit

        method marshaller : marshaller
      end
    val crash_test : unit -> unit
  end
ENDIF
