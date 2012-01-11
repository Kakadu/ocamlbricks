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

module Log = Ocamlbricks_log

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
    let action () =
      if Lazy.lazy_is_val ht then
	let ht = Lazy.force ht in
	begin
	  try
	    let exit_function = Hashtbl.find ht key in
	    let () = exit_function () in
	    Log.printf "Thread Exiting: some thunks executed\n";
	    Hashtbl.remove ht key
	  with Not_found ->
	    Log.printf "Thread Exiting: nothing to do\n"
	end
      else
        Log.printf "Thread Exiting: nothing to do\n"
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
      let action () =
        if Lazy.lazy_is_val ht then begin
          let ht = Lazy.force ht in
          let (actions, exo_actions) = (ref 0, ref 0) in
          (* Executes all thunks related to the *same* process: *)
	  Hashtbl.iter
	    (fun (pid', id') thunk ->
	       if pid=pid' then begin
	         incr actions;
	         (if id<>id' then incr exo_actions);
	         thunk ()
	         end)
	    ht;
	  Hashtbl.clear ht;
	  if !actions = 0
	    then Log.printf "Thread Exiting (main): nothing to do\n"
	    else Log.printf "Thread Exiting (main): %d thunk(s) executed (%d exogenous)\n" !actions ! exo_actions
	  end
	else
          Log.printf "Thread Exiting (main): nothing to do\n"
      in
      apply_with_mutex action ()
    in
    (* Registering here, this action will be executed only by the main thread: *)
    Pervasives.at_exit mrproper

end


module Available_signals = struct

  (* We will use signals from SIGRTMIN (34) to SIGRTMAX (64) included: *)
  module Sem = Semaphore.Array_or (struct let dim = 64 - 34 + 1 end)

  (* For managing both the thread -> signal mapping and the thread -> thunk one: *)
  module Map = MapExtra.Destructive.Int_map
  
  let all_usable_signals =
    Array.to_list (Array.init 31 (fun i -> i+34))

  (* The main structure of this module is an array of semaphores (with the "or" semantics).
     Each forked process must recreate its own fresh structure. *)
  module T = Stateful_modules.Process_private_thread_shared_variable (struct
    type t = (Semaphore.t array) * (int Map.t)
    let name = None
    let init () =
      let semaphores = Sem.create ~init:(Array.make Sem.dim 1) () in
      let mapping    = Map.create () in
      (semaphores, mapping)
  end)

  module EMutex = MutexExtra.EMutex
  
  (* The secondary structure of this module is the container where threads
     may provide themselves a mean to kill them. *)
  module H = Stateful_modules.Process_private_thread_shared_variable (struct
    type t = (unit->unit) Map.t
    let name = None
    let init () = Map.create ()
  end)

  let set_killable_with_thunk ?(who=(Thread.self ())) thunk =
    let mapping = H.extract () in
    let id = Thread.id who in
    let () = H.apply_with_mutex (Map.add id thunk) mapping in
    ()

  let child_remove_thunk_for_killing_me_if_any () =
    let mapping = H.extract () in
    let id = Thread.id (Thread.self ()) in
    let () = H.apply_with_mutex (Map.remove id) mapping in
    ()

  (* Called by the father: *)
  let father_acquire_signal_slot () =
    let (semaphores, mapping) = T.extract () in
    let (i,n) = Sem.p semaphores in (* n=1 *)
    i

  (* Called by the child: *)
  let child_take_possession_of_signal_slot i =
    let (semaphores, mapping) = T.extract () in
    let id = Thread.id (Thread.self ()) in
    let () = T.apply_with_mutex (Map.add id (i+34)) mapping in
    ()

  let child_release_signal_slot i =
    let (semaphores, mapping) = T.extract () in
    let id = Thread.id (Thread.self ()) in
    let () = T.apply_with_mutex (Map.remove id) mapping in
    let () = Sem.v ~i ~n:1 semaphores in
    ()

  let killall () =
    (* Looking in the primary structure: *)
    let (semaphores, mapping) = T.extract () in
    let pid = Unix.getpid () in
    let () = T.apply_with_mutex (Map.iter (fun _ s -> Unix.kill pid s)) mapping in
    (* Looking in the secondary structure: *)
    let mapping = H.extract () in
    let () = H.apply_with_mutex (Map.iter (fun _ thunk -> try thunk () with _ -> ())) mapping in
    ()

  let id_kill_by_signal id =
    Log.printf "Attempting to kill the thread %d by signal...\n" id;
    let (semaphores, mapping) = T.extract () in
    let result = T.apply_with_mutex
     (fun () ->
	try
	  let s = Map.find id mapping in
	  let pid = Unix.getpid () in
	  let () = Unix.kill pid s in
	  true
	with Not_found -> false)
      ()
    in
    result

  let id_kill_by_thunk id =
    Log.printf "Attempting to kill the thread %d by thunk...\n" id;
    let mapping = H.extract () in
    let result = H.apply_with_mutex
     (fun () ->
	try
	  let thunk = Map.find id mapping in
	  (try thunk (); true with _ -> false)
	with Not_found -> false)
      ()
    in
    result

  let id_kill id =
    Log.printf "Attempting to kill the thread %d...\n" id;
    let result = (id_kill_by_signal id) || (id_kill_by_thunk id) in
    Log.printf "Thread %d killed: %b\n" id result;
    result
    
  let kill t = id_kill (Thread.id t)

  let killable () =
    let (semaphores, mapping) = T.extract () in
    let xs = T.apply_with_mutex Map.domain mapping in
    let mapping = H.extract () in
    let ys = H.apply_with_mutex Map.domain mapping in
    List.append xs ys
    
  let id_killer_by_signal id =
    let (semaphores, mapping) = T.extract () in
    let s = T.apply_with_mutex (fun () -> try Some (Map.find id mapping) with Not_found -> None) () in
    match s with
    | Some s ->
        let pid = Unix.getpid () in
        (fun () -> Unix.kill pid s)
    | None   -> raise Not_found

  let id_killer_by_thunk id =
    let mapping = H.extract () in
    let thunk = H.apply_with_mutex (fun () -> try Some (Map.find id mapping) with Not_found -> None) () in
    match thunk with
    | Some thunk -> thunk
    | None -> raise Not_found

  (* The result of the partial application may be transmitted to another process: *)
  let id_killer id =
    let result = 
      try id_killer_by_signal id
      with Not_found -> id_killer_by_thunk id
    in
    let pid = Unix.getpid () in
    Log.printf "Built a killer thunk able to kill %d.%d\n" pid id;
    result

  let killer t = id_killer (Thread.id t)

  let delayed_kill s t =
    ignore (Thread.create (fun () -> Thread.delay s; kill t) ())

  let delayed_killall s =
    ignore (Thread.create (fun () -> Thread.delay s; killall ()) ())

  let delayed_id_kill s id =
    ignore (Thread.create (fun () -> Thread.delay s; id_kill id) ())

  exception Has_been_killed

  (* For the main thread only: register the action of killing all suspending sub-threads.
     This action will provoke the execution of at_exit() for each sub-thread. *)
  let () = 
    Pervasives.at_exit
      (fun () ->
         Log.printf "Thread Exiting (main): killing all sub-threads...\n";
         killall ();
         Thread.delay 0.5 (* Give to the sub-threads the time to perform their at_exit functions *)
         )

end (* module Available_signals *)

(** Similar to [Thread.create] but the result is a triple [(t,k,s)] where [k] is a thunk able to kill the thread and
    [s] is the signal number used by the thunk. This number may be provided to an external process in order to kill
    the thread. In the same process the thunk should be sufficient for this purpose. Note that there are only 31
    (64-34+1) possible threads per process that may run simultaneously with the capability of being killed.
    Thus, this call is blocking: the caller wait until a "signal slot" became available for the thread that
    will be created. *)
let create_killable =
  let handler id s =
    let id' = Thread.id (Thread.self ()) in
    (if id <> id' then
       Log.printf ~v:0 "Wrong behaviour: thread %d should be killed by signal #%d but I'm killed instead\n" id s);
    Log.printf "Killed by signal #%d\n" s;
    raise Available_signals.Has_been_killed
  in
  fun f x ->
    (* The *father* thread executes the following lines: *)
    let i = Available_signals.father_acquire_signal_slot () in
    let s = 34 + i in
    let _ = Thread.sigmask Unix.SIG_BLOCK [s] in
    (* The *child* thread executes the following lines: *)
    let f' y =
      (* Bloc all signals except the owned: *)
      let _ = Thread.sigmask Unix.SIG_SETMASK Available_signals.all_usable_signals in
      let _ = Thread.sigmask Unix.SIG_UNBLOCK [s] in
      let id = Thread.id (Thread.self ()) in
      let previous_handler = Sys.signal s (Sys.Signal_handle (handler id)) in
      let () = Available_signals.child_take_possession_of_signal_slot i in
      Log.printf "Signal #%d reserved to be able to kill this thread\n" s;
      let final_actions () =
        (* The thread should make free the owned signal: *)
        (Sys.set_signal s previous_handler);
        Available_signals.child_release_signal_slot i;
        Available_signals.child_remove_thunk_for_killing_me_if_any ();
        Exit_function.do_at_exit ()
      in      
      try
        let result = f y in
        (final_actions ());
        result
      with e -> begin
        (final_actions ());
        Log.print_exn ~prefix:"Terminated by uncaught exception: " e;
        let () = Thread.exit () in
        (* Not really executed: *)
        raise e
      end
    in
    let thread = Thread.create f' x in
    thread


(** Similar to [Thread.create] but you must call this function if you want to use [ThreadExtra.at_exit] in your thread. *)
let create_non_killable f x =
    let final_actions () =
      Available_signals.child_remove_thunk_for_killing_me_if_any ();
      Exit_function.do_at_exit ()
    in
    let f' y =
      try
        let result = f y in
        (final_actions ());
        result
      with e -> begin
        (final_actions ());
        Log.print_exn ~prefix:"Terminated by uncaught exception: " e;
        let () = Thread.exit () in
        (* Not really executed: *)
        raise e
      end
    in
    Thread.create f' x

let create ?killable f x =
  match killable with
  | None -> create_non_killable f x
  | Some () -> create_killable f x


(* Adapted from standard library unix.ml *)
let rec waitpid_non_intr pid =
  try
    ignore (Unix.waitpid [] pid)
  with
    | Unix.Unix_error (Unix.EINTR, _, _) -> waitpid_non_intr pid
    | Unix.Unix_error (_, _, _) -> ()


let tutor ?killable ?behaviour () =
  let tutor_behaviour =
    let tutor_preamble pid =
      Log.printf "Thread created for tutoring process %d\n" pid;
      Exit_function.at_exit
	(fun () ->
	  Log.printf "Killing tutored process %d...\n" pid;
	  Unix.kill pid 15);
      ()
    in
    match behaviour with
    | None   ->
	(function pid ->
	  tutor_preamble pid;
	  waitpid_non_intr pid
	  )
    | Some behaviour ->
	(function pid ->
	  tutor_preamble pid;
	  let () = behaviour ~pid in
	  waitpid_non_intr pid
	  )
  in
  fun ~pid -> create ?killable tutor_behaviour pid


let fork ?killable ?behaviour f x =
  let tutor = tutor ?killable ?behaviour () in
  let pid = Unix.getpid () in
  let id = Thread.id (Thread.self ()) in
  let thread =
    match Unix.fork () with
    | 0 ->
	(* The child here: *)
	begin
	  Log.printf "Process (fork) created by %d.%d\n" pid id;
	  let _ =
	    try f x
            with e ->
              Log.print_exn ~prefix:"Terminated by uncaught exception: " e;
              raise e
	  in
	  let () = exit 0 in
	  raise Not_found (* not really executed, just to get around the type system *)
	end
    | child_pid ->
	(* The father here creates a process-tutor thread per child: *)
(* 	create ?killable process_tutor child_pid *)
	tutor child_pid
  in
  thread
;;

(* In order to render killall and kill directly accessible at this level: *)
include Available_signals
include Exit_function
(* let at_exit = Exit_function.at_exit *)

