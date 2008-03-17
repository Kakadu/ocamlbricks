(* This file is part of our reusable OCaml BRICKS library
   Copyright (C) 2007  Jean-Vincent Loddo

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

(** Module for building memoised functions. *)

(* The string of a key *)
let sok x = string_of_int (Hashtbl.hash x) ;;

(* The class storing the hash table *)
class ['a,'b] memo = fun ?(trace=false) ?(size=64) () ->

  object (self) 

  (** The state of the hash table. *)
  val current : ('a,'b) Hashtbl.t = (Hashtbl.create size)
  val trace   = trace

  (** Get the current encapsulated value a given input or call the function. *)
  method get (f:'a->'b) (x:'a) = try 
	begin
	 let y=(Hashtbl.find current x) in 
         (if trace then (prerr_endline ("Memo.call: value found for key "^(sok x))); y) 
	end
     with Not_found -> 
	begin
         if trace then prerr_endline ("Memo.call: using function for key "^(sok x)); 
      	 let y = (f x) in 
	  ((Hashtbl.replace current x y); y)
     	end

end;;

(** The abstract type of memoisation tables. *)
type ('a,'b) t = ('a,'b) memo;;

(** The constructor of memoisation tables.*)
let make ?(trace=false) ?(size=64) () : ('a,'b) t = new memo ~trace ~size ();;

(** The call of a function supervised by a memoisation table.*)
let call (mt:('a,'b) t) (f:('a->'b)) x = mt#get f x;;

