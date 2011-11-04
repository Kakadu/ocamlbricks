(* This file is part of our reusable OCaml BRICKS library
   Copyright (C) 2009  Luca Saiu
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

(* Authors:
 * - Jean-Vincent Loddo: complete rewriting, functorization, recursive_mutex as internal module
 * - Luca Saiu: initial version
 *)

(* Do not remove the following comment: it's an ocamldoc workaround. *)
(** *)

(** Make extra definitions for Mutex. *)
module Extend
 (Mutex : sig
  type t 
  val create   : unit -> t
  val lock     : t -> unit
  val unlock   : t -> unit
 end) = struct

 include Mutex
 
(** Execute thunk in a synchronized block, and return the value returned
    by the thunk. If executing thunk raises an exception the same exception
    is propagated, after correctly unlocking the mutex. *)
 let with_mutex mutex thunk =
  Mutex.lock mutex;
  try
    let result = thunk () in
    Mutex.unlock mutex;
    result
  with e -> begin
    Mutex.unlock mutex;
    (Printf.eprintf
      "MutexExtra.Make.with_mutex: exception %s raised in critical section. Unlocking and re-raising.\n"
      (Printexc.to_string e));
    raise e;
  end

 (** Similar to [with_mutex]: the argument will be given to the function in a synchronized block. *)
 let apply_with_mutex mutex f x =
  let thunk () = f x in
  with_mutex mutex thunk

end


(** Extended simple mutexes. *)
module Extended = Extend(Mutex)

(** A simple implementation of recursive mutexes inspired by Luca's version and by
    a (bugged?) version found in the project http://batteries.forge.ocamlcore.org/.
    In my opinion there's a bug in the batteries' version that I think fixed here
    (see comments about a possible ownership "stole") -- Jean-Vincent.
    *)
module Recursive_base = struct

type owner = {
  thread_id       : int;   (** The thread identifier of the owner *)
  mutable lock_no : int;   (** Number of lock performed by the owner (lock_no >= 1) *)
  }
    
type t = {
  waiting_mutex : Mutex.t;      (** The mutex used for passive waiting *)
  owner_mutex   : Mutex.t;      (** The mutex used to protect the access to the owner fields *)
  mutable owner : owner option;
  }
      
let create () = {
  waiting_mutex = Mutex.create ();
  owner_mutex   = Mutex.create ();
  owner         = None
  }

let lock t =
  let id = Thread.id (Thread.self ()) in
  let rec loop ~waiting_mutex_already_locked =
    let am_i_the_owner =
      Extended.apply_with_mutex t.owner_mutex
        (fun () ->
         match t.owner with
         | None ->
             t.owner <- Some {thread_id = id; lock_no = 1};
             (if waiting_mutex_already_locked then () else Mutex.lock t.waiting_mutex);
             true
         | Some x when x.thread_id = id ->
             x.lock_no <- x.lock_no + 1;
             true
        | Some _ when waiting_mutex_already_locked ->
             (* Damn, someone stole my ownership! Ok, I renounce and I will wait again: *)
             Mutex.unlock t.waiting_mutex;
             false
         | Some _ -> false 
         )
    in
    match am_i_the_owner () with
    | true  -> ()
    | false -> begin
        Mutex.lock t.waiting_mutex;
        (* Note: now someone may stole my ownership calling lock ()... *)
        loop ~waiting_mutex_already_locked:true
        end
  in loop ~waiting_mutex_already_locked:false
    
    
let unlock t =
  let id = Thread.id (Thread.self ()) in
  Extended.with_mutex t.owner_mutex
    (fun () ->
     match t.owner with
     | Some x when x.thread_id = id ->
	 if x.lock_no > 1
	   then x.lock_no <- x.lock_no - 1
	   else begin
	     t.owner <- None;
	     Mutex.unlock t.waiting_mutex  
	   end
     | _ -> assert false
     )

end

(** Extended recursive mutexes. *)
module Recursive = Extend(Recursive_base)
