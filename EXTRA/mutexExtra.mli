(* This file is part of our reusable OCaml BRICKS library
   Copyright (C) 2009, 2011  Jean-Vincent Loddo
   Copyright (C) 2011  Université Paris 13

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

(** Additional features for the standard module [Mutex].

{b Example}:
{[(* Extend the standard Mutex: *)
module Mutex = MutexExtra.Extend (Mutex);;

(* Use the function with_mutex: *)
Mutex.with_mutex mutex (fun () -> ...);; 

(* Idem for recursive mutexes: *)
module Recursive_mutex = MutexExtra.Extend (MutexExtra.Recursive_mutex) ;;

(* Or equivalently, you can use a predefined functor application as shortcut: *)
module Recursive_mutex = MutexExtra.Recursive ;;
]}
*)

module Extend : functor

  (Mutex : sig
     type t
     val create   : unit -> t
     val lock     : t -> unit
     val unlock   : t -> unit
     val try_lock : t -> bool
  end) ->
  sig
    type t = Mutex.t
    val create : unit -> t
    val status : t -> bool
    val with_mutex       : t -> (unit -> 'a) -> 'a
    val apply_with_mutex : t -> ('a -> 'b) -> 'a -> 'b
  end

module type Extended_signature = 
 sig
   type t

   val create   : unit -> t
   val lock     : t -> unit
   val unlock   : t -> unit
   val try_lock : t -> bool
   val status   : t -> bool

   val with_mutex       : t -> (unit -> 'a) -> 'a
   val apply_with_mutex : t -> ('a -> 'b) -> 'a -> 'b

 end

module EMutex : Extended_signature   (* Extended mutexes *)
module RMutex : Extended_signature   (* Recursive mutexes *)

(* Just a more explicit alias for RMutex: *)
module Recursive : Extended_signature 

module Just_give_me_an_apply_with_mutex : functor (M:sig end) ->
  sig
    val apply_with_mutex : ('a -> 'b) -> 'a -> 'b
  end

