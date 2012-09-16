(* This file is part of our reusable OCaml BRICKS library
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

(** Facility for tracing program activities, specially using threads.
    Essentially, this module provides a function [Log.printf] printing your
    messages only when the current debugging level of your application rises
    above ([>=]) the verbosity. Two ore more modules may be built
    on the same {!Log_builder.log_channel}. Actually:
    - an internal global hash table register the associations [(file,out_channel)]
    - a mutex per out_channel is created in order to preserve atomicity of each printing.

{b Example}:
{[(* Define your log modules: *)
module Log1 = Log_builder.Make (struct
  let debug_level () = ...                (* explain here where to get the current value of the debugging level;
                                             this value must be greater or equal to the verbosity, otherwise do nothing *)
  let verbosity = 1                       (* the default value of verbosity for printing functions *)
  let log_channel = `stderr               (* put messages here *)
  let synchronized = true                 (* using threads *)
 end);;

module Log2 = Log_builder.Make (struct
  let debug_level () = ...
  let verbosity = 2
  let log_channel = `stderr               (* share the same channel of Log1 *)
  let synchronized = true
 end);;

(* Put calls somewhere in your code : *)
...
Log1.printf "%s\n" value;   (* really printed when debug level >= 1 *)
...
Log2.printf "%s\n" value;   (* really printed when debug level >= 2 *)
...
]}
*)

(** Data type representing the output channel where the messages will be written. *)
type log_channel = [ `stdout | `stderr | `file of string ]

(** The signature of the module resulting from functors' applications. *)
module type Result = sig
  (**  The banner is a complement prepended to your message. It contains
       informations about the program and the thread which are executed. {b Example:}
{[# module Log = Log.Make_simple (struct let is_log_enabled () = true end) ;;

# Log.printf "The result is %d\n" 42 ;;
toplevel 13920 [thread 0]: The result is 42
   : unit = ()

# Log.printf ~banner:false "The result is %d\n" 42 ;;
The result is 42
   : unit = ()
]}*)
  val printf        : ?v:int -> ?force:bool -> ?banner:bool -> (('a, out_channel, unit) format) -> 'a
  val print_exn     : ?v:int -> ?force:bool -> ?banner:bool -> ?prefix:string -> ?suffix:string -> exn -> unit
  val print_string  : ?v:int -> ?force:bool -> string -> unit
  val print_int     : ?v:int -> ?force:bool -> int -> unit
  val print_float   : ?v:int -> ?force:bool -> float -> unit
  val print_newline : ?v:int -> ?force:bool -> unit -> unit
  val print_endline : ?v:int -> ?force:bool -> string -> unit

  module Unprotected:sig
  val printf        : ?v:int -> ?force:bool -> ?banner:bool -> (('a, out_channel, unit) format) -> 'a
  val print_exn     : ?v:int -> ?force:bool -> ?banner:bool -> ?prefix:string -> ?suffix:string -> exn -> unit
  end

  module Tuning:sig
     val verbosity      : unit -> int
     val debug_level    : unit -> int
     val is_log_enabled : ?v:int -> unit -> bool
     val log_channel    : log_channel
     val synchronized   : bool
     module Set : sig
       val verbosity : int -> unit
       val debug_level : (unit -> int) -> unit
     end
   end

 end

(** {2 General construction} *)

(** Build a module with printing facilities, providing a "tuning" module which defines
    four parameters:
    - the [threshold] starting from the printing is really performed
    - the thunk [get_current_verbosity] providing the current level of verbosity of the application
    - the {!Log_builder.log_channel} where messages will be put in
    - the flag [synchronized] indicating if threads are in use, hence if synchronizations are required
      to preserve the atomicity of printing operations. *)
module Make :
  functor
    (Tuning : sig
           val verbosity    : int
           val debug_level  : unit -> int
           val log_channel  : log_channel
           val synchronized : bool
         end) -> Result

(** {2 Simplified construction} *)

(** Build a kit of printing functions using just one parameter,
    the thunk [is_log_enabled], that simply indicates if the debugging is currently activated in the application.
    Using this functor, the output channel is set to [stderr] and the synchronization is performed. *)
module Make_simple :
  functor
    (Tuning : sig val is_log_enabled : unit -> bool end) -> Result

(** {b Example}:
{[
(* Initialized later, by Global_options, in order to break the ciclic dependency: *)
module Self = Log_builder.Make (struct
  let debug_level () = 0           (* the debug_level must be greater or equal to the verbosity, otherwise do nothing *)
  let verbosity = 1                (* the default value of verbosity for printing functions *)
  let log_channel = `stderr        (* put messages here *)
  let synchronized = true          (* using threads *)
 end);;

include Log_builder.Extend_with_wrappers (Self) ;;
]}
*)
module Extend_with_wrappers :
  functor (Log : Result) ->
    sig
      include Result

      val system_or_fail :
        ?on_error:UnixExtra.command ->
        ?hide_output:bool -> ?hide_errors:bool -> UnixExtra.command -> unit

      val system_or_ignore :
        ?on_error:UnixExtra.command ->
        ?hide_output:bool -> ?hide_errors:bool -> UnixExtra.command -> unit

      val print_backtrace : unit -> unit
    end
