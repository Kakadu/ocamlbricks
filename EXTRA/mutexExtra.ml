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
 * - Luca Saiu: function with_mutex from recursive_mutex.ml
 * - Jean-Vincent Loddo: functorization, recursive_mutex as internal module
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


(** A simple implementation of recursive mutexes. Non-recursive mutexes are
    a real pain to use: *)
module Recursive_mutex = struct

 type t =
  Mutex.t *               (* the underlying mutex *)
  Mutex.t *               (* a mutex protecting the other fields *)
  (Thread.t option ref) * (* the thread currently holding the mutex *)
  (int ref);;             (* lock counter *)

 (** Create a new recursive mutex: *)
 let create () : t =
  (Mutex.create ()),
  (Mutex.create ()),
  (ref None),
  (ref 0);;

(** Lock a mutex; only block if *another* thread is holding it. *)
 let rec lock (the_mutex, fields_mutex, owning_thread_ref, lock_counter_ref) =
  let my_thread = Thread.self () in
  Mutex.lock fields_mutex;
  match !owning_thread_ref with
    None -> begin
      owning_thread_ref := Some my_thread;
      lock_counter_ref := !lock_counter_ref + 1;
      Mutex.unlock fields_mutex;
      Mutex.lock the_mutex;
    end
  | Some t when t = my_thread -> begin
      lock_counter_ref := !lock_counter_ref + 1;
      Mutex.unlock fields_mutex;
    end
  | Some _ -> begin
      Mutex.unlock fields_mutex;
      (* the_mutex is locked. Passively wait for someone to free it: *)
      flush stderr;
      Mutex.lock the_mutex;
      Mutex.unlock the_mutex;
      (* Try again: *)
      lock (the_mutex, fields_mutex, owning_thread_ref, lock_counter_ref);
    end;;

 (** Unlock a recursive mutex owned by the calling thread. *)
 let unlock (the_mutex, fields_mutex, owning_thread_ref, lock_counter_ref) =
  let _ = Thread.self () in
  Mutex.lock fields_mutex;
  match !owning_thread_ref with
    None ->
      flush_all ();
      assert false;
  | Some t -> begin
      if !lock_counter_ref = 1 then begin
        lock_counter_ref := 0;
        owning_thread_ref := None;
        Mutex.unlock fields_mutex;
        Mutex.unlock the_mutex;
      end
      else begin
        lock_counter_ref := !lock_counter_ref - 1;
        Mutex.unlock fields_mutex;
      end
    end;;

end

(** Extended recursive mutexes. *)
module Recursive = Extend(Recursive_mutex)
