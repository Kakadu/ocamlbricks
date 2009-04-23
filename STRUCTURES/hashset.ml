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

(* Do not remove the following comment: it's an ocamldoc workaround. *)
(** *)

(** The default size of the hash used in the implementation *)
let default_size = 251;;

class ['a] hashset = fun ?(size=default_size) () ->

  object (self) 

  (** The state of the hashset. *)
  val current : ('a, unit) Hashtbl.t = (Hashtbl.create size)

  (** Answer (quickly!) to the question if x is a member of the set. *)
  method mem x = Hashtbl.mem current x

  (** Add the element to the set *)
  method add x = (Hashtbl.replace current x ()) 

  (** Remove the element from the set *)
  method remove x = (Hashtbl.remove current x) 

end;; (* class hashset *)


(* Functional interface. *)

(** The abstract type of an hashset. *)
type 'a t       = 'a hashset ;;
 
(** The hashset constructor. *)
let make ?(size=default_size) () : 'a t = new hashset ~size () ;;

(** The member predicate. *)
let mem (hs:'a t) (x:'a) = hs#mem x;;

(** Add a member to the hashset. *)
let add (hs:'a t) (x:'a) = hs#add x;;
 
(** Remove a member from the hashset. *)
let remove (hs:'a t) (x:'a) = hs#remove x;;
 
(** Make an hashset from a list. *)
let of_list (l:'a list) : 'a t = 
 let n = List.length l in 
 let size = if n<(default_size/2) then default_size else n*2 in
 let hs = make ~size () in 
 (List.iter (add hs) l); hs
;;

(* Support for uniq. *)
let rec uniq hs = function
 | []   -> []
 | x::l -> if (hs#mem x) then (uniq hs l) else ((hs#add x); (x::(uniq hs l)))   
;;

(** Exploit an hashset for implementing the uniq function over lists. *)
let uniq (l:'a list) : ('a list) =
 let n = List.length l in
 let hs = make ~size:(n*3) () in
 uniq hs l
;;

