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

    method register_functorized_object_field : 
      ?name:string -> 'd -> 'e -> 'f -> 'g -> unit
    
    method register_object_field : 
      ?name:string -> 
      (unit -> (< marshaller : marshaller; .. > as 'h)) ->
      ('h -> unit) -> unit
    
    method register_simple_field :
      ?name:string -> (unit -> 'i) -> ('i -> unit) -> unit
    
    method parent_class_name : string option
    
    method protected_load_from_string_in_a_context : Unmarshalling_env.t -> string -> unit * Unmarshalling_env.t
    method protected_save_to_string_in_a_context   : Marshalling_env.t   -> string * Marshalling_env.t
    
  end

  
val marshallable_classes_version  : string
val marshallable_classes_metadata : unit -> string

val enable_warnings  : unit -> unit
val disable_warnings : unit -> unit

val enable_tracing  : unit -> unit
val disable_tracing : unit -> unit


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
        method marshaller : marshaller
      end
    val crash_test : unit -> unit
  end
ENDIF