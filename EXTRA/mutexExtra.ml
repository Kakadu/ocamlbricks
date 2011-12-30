(* This file is part of our reusable OCaml BRICKS library
   Copyright (C) 2009, 2011  Jean-Vincent Loddo
   Copyright (C) 2009  Luca Saiu
   Copyright (C) 2009, 2011  Université Paris 13

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
 * - Jean-Vincent Loddo: complete rewriting (2011), functorization (2009)
 * - Luca Saiu: initial version
 *)

(* Do not remove the following comment: it's an ocamldoc workaround. *)
(** *)

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

(** Make extra definitions for Mutex. *)
module Extend
 (Mutex : sig
  type t 
  val create   : unit -> t
  val lock     : t -> unit
  val unlock   : t -> unit
  val try_lock : t -> bool
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
      "MutexExtra.Extend.with_mutex: exception %s raised in critical section. Unlocking and re-raising.\n"
      (Printexc.to_string e));
    raise e;
  end

 (** Similar to [with_mutex]: the argument will be given to the function in a synchronized block. *)
 let apply_with_mutex mutex f x =
  let thunk () = f x in
  with_mutex mutex thunk

 (** Similar to try_lock but the mutex is not locked (useful for monitoring). In this quick-and-easy
     implementation we call first try_lock, then unlock if necessary: *)
 let status mutex =
  let result = try_lock mutex in
  if result then unlock mutex else ();
  result

end


(** Extended simple mutexes. *)
module EMutex = Extend(Mutex)

(** A simple implementation of recursive mutexes inspired by Luca's version and by
    a (bugged?) version found in the project http://batteries.forge.ocamlcore.org/.
    In my opinion there's a bug in the batteries' version that I think fixed here
    using a condition variable instead of a second mutex -- Jean-Vincent.
    *)
module Recursive_base = struct

type owner = {
  thread_id       : int;   (** The thread identifier of the owner *)
  mutable lock_no : int;   (** Number of lock performed by the owner (lock_no >= 1) *)
  }

type t = {
  waiting_condition : Condition.t;  (** The condition variable used for passive waiting *)
  owner_mutex   : Mutex.t;          (** The mutex used to protect the access to the owner fields *)
  mutable owner : owner option;
  }

let create () = {
  waiting_condition = Condition.create ();
  owner_mutex       = Mutex.create ();
  owner             = None
  }

let lock t =
  let id = Thread.id (Thread.self ()) in
  EMutex.with_mutex t.owner_mutex
    (fun () ->
      match t.owner with
      | None ->
          t.owner <- Some {thread_id = id; lock_no = 1}

      | Some x when x.thread_id = id ->
          x.lock_no <- x.lock_no + 1

      | Some x ->
	  begin
	    while not (t.owner = None) do
	      Condition.wait t.waiting_condition t.owner_mutex
	    done;
	    t.owner <- Some {thread_id = id; lock_no = 1};
	  end
      )


let try_lock t =
  let id = Thread.id (Thread.self ()) in
  EMutex.with_mutex t.owner_mutex
    (fun () ->
      match t.owner with
      | None ->
          t.owner <- Some {thread_id = id; lock_no = 1};
          true

      | Some x when x.thread_id = id ->
          x.lock_no <- x.lock_no + 1;
          true

      | Some x ->
	  false
      )


let unlock t =
  let id = Thread.id (Thread.self ()) in
  EMutex.with_mutex t.owner_mutex
    (fun () ->
     match t.owner with
     | Some x when x.thread_id = id ->
	 if x.lock_no > 1
	   then x.lock_no <- x.lock_no - 1
	   else begin
	     t.owner <- None;
	     (Condition.signal t.waiting_condition);
	   end
     | _ -> invalid_arg "Trying to unlock a not owned recursive mutex"
     )


(* More efficient implementation (see comment above about Extend.status): *)
let status t =
  let id = lazy (Thread.id (Thread.self ())) in
  EMutex.with_mutex t.owner_mutex
    (fun () ->
      match t.owner with
      | None -> true
      | Some x when x.thread_id = (Lazy.force id) -> true
      | Some x -> false
      )

end (* Recursive_base *)


(** Extended recursive mutexes. *)
module RMutex = struct
 include Extend(Recursive_base)

 (* Redefined for efficiency: *)
 let status = Recursive_base.status

end

module Recursive = RMutex

(** Usage: {[ include MutexExtra.RMutex.Just_give_me_an_apply_with_mutex (struct end) ]} *)
module Just_give_me_an_apply_with_mutex (M:sig end) = struct
  let mutex = RMutex.create ()
  let apply_with_mutex f x = RMutex.apply_with_mutex mutex f x
end
