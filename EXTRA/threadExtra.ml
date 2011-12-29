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

(* From SIGRTMIN (34) to SIGRTMAX (64) included *)
module Available_signals = Semaphore.Array_or (struct let dim = 64 - 34 + 1 end)

let signal_list =
  Array.to_list (Array.init 31 (fun i -> i+34))

let available_signals =
  let init = Array.make Available_signals.dim 1 in
  Available_signals.create ~init ()

exception Has_been_killed

(** Similar to [Thread.create] but the result is a triple [(t,k,s)] where [k] is a thunk able to kill the thread and
    [s] is the signal number used by the thunk. This number may be provided to an external process in order to kill
    the thread. In the same process the thunk should be sufficient for this purpose. *)
let create_killable ?verbose =
  let handler id s =
    let id' = Thread.id (Thread.self ()) in
    (if id <> id' then Printf.kfprintf flush stderr "Wrong behaviour: thread %d should be killed by signal %d but thread %d is killed instead\n" id s id');
    (if verbose = Some () then Printf.kfprintf flush stderr "Thread %d killed by signal %d\n" id' s);
    raise Has_been_killed
  in
  fun f x ->
    let (i,n) = Available_signals.p available_signals in
    let s = 34 + i in
    let _ = Thread.sigmask Unix.SIG_BLOCK [s] in
    let f' y =
      (* Bloc all signals except the owned: *)
      let _ = Thread.sigmask Unix.SIG_SETMASK signal_list in
      let _ = Thread.sigmask Unix.SIG_UNBLOCK [s] in
      let id = Thread.id (Thread.self ()) in
      let previous_handler = Sys.signal s (Sys.Signal_handle (handler id)) in
      (if verbose = Some () then Printf.kfprintf flush stderr "Thread %d owning the signal %d\n" id s);
      try
        let result = f y in
        (* The thread "free" the signal: *)
        Available_signals.v ~i ~n available_signals;
        let () = Sys.set_signal s previous_handler in
        result
      with e -> begin
        Available_signals.v ~i ~n available_signals;
        let () = Sys.set_signal s previous_handler in
        let () = Thread.exit () in
        (* Not really executed: *)
        raise e
      end
    in
    let kill_function = fun () -> Unix.kill (Unix.getpid ()) s in
    let thread = Thread.create f' x in
    (thread, kill_function, s)

