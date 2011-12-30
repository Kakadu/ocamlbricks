(* This file is part of ocamlbricks
   Copyright (C) 2011 Jean-Vincent Loddo

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

(** Additional features for the module [Thread] provided by the [threads] library. *)

module Exit_function = struct

  include MutexExtra.Just_give_me_an_apply_with_mutex (struct end)

  (* The hash table will be really built on demand: *)
  let ht = lazy (Hashtbl.create 51)

  let at_exit thunk =
    let pid = Unix.getpid () in
    let id = Thread.id (Thread.self ()) in
    let key = (pid, id) in
    let protected_thunk () = try thunk () with _ -> () in
    let ht = Lazy.force ht in
    let action () =
      let exit_function =
        try Hashtbl.find ht key with Not_found -> (fun ()->())
      in
      let exit_function = (fun () -> protected_thunk (); exit_function ()) in
      Hashtbl.replace ht key exit_function
    in
    apply_with_mutex action ()

  let do_at_exit () =
    let pid = Unix.getpid () in
    let id = Thread.id (Thread.self ()) in
    let key = (pid, id) in
    Printf.kfprintf flush stderr "Thread %d.%d has called do_at_exit\n" pid id;
    let action () =
      if Lazy.lazy_is_val ht then
       let ht = Lazy.force ht in
       begin
	 try
	   let exit_function = Hashtbl.find ht key in
	   let () = exit_function () in
	   Hashtbl.remove ht key
	 with Not_found ->
	   (Printf.kfprintf flush stderr "Nothing to do for the thread %d.%d\n" pid id);
	   ()
       end
    in
    apply_with_mutex action ()


  (* Register a main thread final action performing all remaining registered functions for the same process.
     Note that the main thread has the identifier 0 only for the main process. For the child processes the
     main thread identifier has the value of the father thread in the father process.
     For instance, a Unix.fork() called in the thread 1000.6 (1000 is the pid) could create a main thread
     like 1042.6. *)
  let () =
    let mrproper () =
      let pid = Unix.getpid () in
      let id = Thread.id (Thread.self ()) in
      Printf.kfprintf flush stderr "The main thread %d.%d has called mrproper\n" pid id;
      let action () =
        if Lazy.lazy_is_val ht then begin
          Printf.kfprintf flush stderr "The main thread %d.%d is performing mrproper...\n" pid id;
          let ht = Lazy.force ht in
          (* Executes all thunks related to the *same* process: *)
	  Hashtbl.iter (fun (pid', _) thunk -> if pid=pid' then thunk ()) ht;
	  Hashtbl.clear ht
	  end
      in
      apply_with_mutex action ()
    in
    (* Registering here, this action will be executed only by the main thread: *)
    Pervasives.at_exit mrproper

end

let at_exit f = Exit_function.at_exit f

module Available_signals = struct

  (* We will use signals from SIGRTMIN (34) to SIGRTMAX (64) included: *)
  module Sem = Semaphore.Array_or (struct let dim = 64 - 34 + 1 end)
  
  let all_usable_signals =
    Array.to_list (Array.init 31 (fun i -> i+34))

  (* The main structure of this module is an array of semaphores (with the "or" semantics).
     Each forked process must recreate its own fresh structure. *)
  module T = Stateful_modules.Process_private_thread_shared_variable (struct
    type t = Semaphore.t array
    let name = None
    let init () = Sem.create ~init:(Array.make Sem.dim 1) ()
  end)

  let acquire () = Sem.p (T.extract ())
  let release ~i ~n = Sem.v ~i ~n (T.extract ())
  
  exception Has_been_killed

end (* module Available_signals *)

(** Similar to [Thread.create] but the result is a triple [(t,k,s)] where [k] is a thunk able to kill the thread and
    [s] is the signal number used by the thunk. This number may be provided to an external process in order to kill
    the thread. In the same process the thunk should be sufficient for this purpose. Note that there are only 31
    (64-34+1) possible threads per process that may run simultaneously with the capability of being killed.
    Thus, this call is blocking: the caller wait until a "signal slot" became available for the thread that
    will be created. *)
let create_killable ?verbose =
  let handler id s =
    let id' = Thread.id (Thread.self ()) in
    (if id <> id' then Printf.kfprintf flush stderr "Wrong behaviour: thread %d should be killed by signal %d but thread %d is killed instead\n" id s id');
    (if verbose = Some () then Printf.kfprintf flush stderr "Thread %d killed by signal %d\n" id' s);
    raise Available_signals.Has_been_killed
  in
  fun f x ->
    let (i,n) = Available_signals.acquire () in
    let s = 34 + i in
    let _ = Thread.sigmask Unix.SIG_BLOCK [s] in
    let f' y =
      (* Bloc all signals except the owned: *)
      let _ = Thread.sigmask Unix.SIG_SETMASK Available_signals.all_usable_signals in
      let _ = Thread.sigmask Unix.SIG_UNBLOCK [s] in
      let id = Thread.id (Thread.self ()) in
      let previous_handler = Sys.signal s (Sys.Signal_handle (handler id)) in
      (if verbose = Some () then Printf.kfprintf flush stderr "Thread %d owning the signal %d\n" id s);
      let final_actions () =
        (* The thread should make free the owned signal: *)
        Available_signals.release ~i ~n;
        (Sys.set_signal s previous_handler);
        Exit_function.do_at_exit ()
      in      
      try
        let result = f y in
        (final_actions ());
        result
      with e -> begin
        (final_actions ());
        let () = Thread.exit () in
        (* Not really executed: *)
        raise e
      end
    in
    let kill_function = fun () -> Unix.kill (Unix.getpid ()) s in
    let thread = Thread.create f' x in
    (thread, kill_function, s)


(** Similar to [Thread.create] but you must call this function if you want to use [ThreadExtra.at_exit] in your thread. *)
let create f x =
    let f' y =
      try
        let result = f y in
        Exit_function.do_at_exit ();
        result
      with e -> begin
        Exit_function.do_at_exit ();
        let () = Thread.exit () in
        (* Not really executed: *)
        raise e
      end
    in
    Thread.create f' x

