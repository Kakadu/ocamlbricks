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

(** Build and manage unique (fresh) identifiers. *)

(** Some predefined GLOBAL and independent counters. 
    The first may be used, for example, for generating fresh identifiers, 
    the second for generating tickets (time marks), and so on *)
let counter = [| 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; |] 
;;

(** Reset the global counter with the given index *)
let reset ?(index=0) () = counter.(index) <- 0 
;;

(** Return a fresh unused value, using the given counter index, which is updated *)
let click ?(index:int=0) () =
   let id = (counter.(index) + 1)   in 
   let _  = (counter.(index) <- id) in id 
;; 

(** Return a fresh unused id (using the counter index 0) *)
let fresh  = click ~index:0

(** Return a fresh unused ticket which may be used, for instance, as time mark (using the counter index 1) *)
let ticket = click ~index:1


(** Allocating a new local counter and its related fresh function. *)
let makefresh () = 
  let counter = ref 0 in 
  let fresh = (fun () -> 
   let id = (!counter + 1)  in 
   let _  = (counter := id) in id) in fresh
;;

