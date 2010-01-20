(* This file is part of our reusable OCaml BRICKS library
   Copyright (C) 2008  Luca Saiu
   Copyright (C) 2010  Jean-Vincent Loddo

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
 * - Jean-Vincent Loddo: migration from marionnet, synchronization, functorization 
 * - Luca Saiu: Original code in marionnet/log.ml
 *)

(* Do not remove the following comment: it's an ocamldoc workaround. *)
(** *)

(** Data type representing the output channel where the messages will be written. *)
type log_channel =
 | Stdout
 | Stderr
 | File of string

(** The signature of the module resulting from functors' applications. *)
module type Result = sig
  val printf : ?banner:bool -> (('a, out_channel, unit) format) -> 'a
  val print_string  : string -> unit
  val print_int     : int -> unit
  val print_float   : float -> unit
  val print_newline : unit -> unit
  val print_endline : string -> unit
 end
 
(* The out channels are shared by all threads of the program. Hence, there is a mutex
   per channel. Almost two mutex are created loading this module: one for stderr and one for stdout.
   File "/dev/stdout" (resp. File "/dev/stderr") is equivalent to Stdout (resp. Stderr).
   *)
let get_out_channel =
 let ht = Hashtbl.create 51 in
 let stdout_mutex = Mutex.create () in
 let stderr_mutex = Mutex.create () in
 let () = Hashtbl.add ht "/dev/stdout" (stdout, stdout_mutex) in
 let () = Hashtbl.add ht "/dev/stderr" (stderr, stderr_mutex) in
 function
  | Stdout -> (stdout, stdout_mutex)
  | Stderr -> (stderr, stderr_mutex)
  | File f -> try Hashtbl.find ht f
     with
      Not_found ->
       begin
        let out_channel = open_out f in
        let mutex = Mutex.create () in
        (Hashtbl.add ht f (out_channel,mutex));
        (out_channel, mutex)
       end


module Make
 (Tuning:sig
     val threshold   : int
     val get_current_verbosity : unit -> int
     val log_channel     : log_channel
     val synchronized    : bool
   end) : Result =
 struct

  let (out_channel, mutex) = get_out_channel Tuning.log_channel
  let get_debug_enabled () = (Tuning.get_current_verbosity ()) >= Tuning.threshold

  let apply_with_mutex (f:'a -> 'b) (x:'a) : 'b =
   let module Mutex = MutexExtra.Extend (Mutex) in
   Mutex.apply_with_mutex mutex f x

  (* Take a format string and either use it for Printf.printf, or use it
     for a dummy printf-like function which does nothing, according to
     whether we're in debug mode or not: *)
  let printf_unsynchronized ~banner frmt =
    Obj.magic
      (if get_debug_enabled () then
       begin
         (match banner with
           | false -> ()
           | true  ->
              let thread_id = Thread.id (Thread.self ()) in
              let program   = Filename.basename (Sys.executable_name) in
              let pid = Unix.getpid () in
              let prefix = Printf.sprintf "%s %d [thread %d]: " program pid thread_id in
              Printf.kfprintf flush out_channel "%s" prefix);
         Printf.kfprintf flush out_channel frmt
       end
      else
        Printf.ifprintf out_channel frmt)

  let printf ?(banner=true) frmt =
   if not Tuning.synchronized
    then printf_unsynchronized ~banner frmt
    else apply_with_mutex (printf_unsynchronized ~banner) frmt


  (* Here Obj.magic just avoids a warning "Warning X: this argument will not be used by the function.".
     For a misunderstood reason, we must define and call the function printf_nobanner into Obj.magic.
     Otherwise the banner is printed... *)
  let printf_nobanner = printf ~banner:false
  let print_string  x  = (Obj.magic printf_nobanner (format_of_string "%s")) x
  let print_string  x  = (Obj.magic printf_nobanner (format_of_string "%s")) x
  let print_int     x  = (Obj.magic printf_nobanner (format_of_string "%d")) x
  let print_float   x  = (Obj.magic printf_nobanner (format_of_string "%f")) x
  let print_endline x  = (Obj.magic printf_nobanner (format_of_string "%s\n")) x
  let print_newline () = (Obj.magic printf_nobanner (format_of_string "\n"))

end

module Make_simple (Tuning:sig val is_log_enabled : unit -> bool end) =
 Make
 (struct
     let threshold = 1
     let get_current_verbosity () = if Tuning.is_log_enabled () then 1 else 0
     let log_channel     = Stderr
     let synchronized    = true
   end)

