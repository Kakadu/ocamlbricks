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

(** Additional features for the standard module [Stack].

{b Usage}:
-    {[ open StackExtra;; ]}
-    {[ module Stack = StackExtra.Stack;; ]}
The previous phrases are equivalent and allow you to access to additional features for stacks.

You can give a look to the {!StackExtra.Stack} module documentation for more informations on these features.

Essentially, this module is defined just to obtain a function [to_list] with complexity O(1).
*)

(** Replacement for the standard [Stack]. *)
module Stack = struct

 type 'a t = { mutable l : 'a list }
 exception Empty
 let create () = { l = [] }
 let clear s = s.l <- []
 let copy s = { l = s.l }
 let push x s = s.l <- x :: s.l
 let pop s = match s.l with x::xs -> s.l <- xs; x | [] -> raise Empty
 let top s = match s.l with x::_  -> x            | [] -> raise Empty
 let is_empty s = (s.l = [])
 let length s = List.length s.l
 let iter f s = List.iter f s.l
 
 (** The unique difference with the standard [Stack] is this function that transforms the stack in a list in O(1).  *)
 let to_list s = s.l
end;;

