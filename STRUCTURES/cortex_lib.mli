(* This file is part of ocamlbricks
   Copyright (C) 2013  Jean-Vincent Loddo

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


(** Common recurrent cortex instances (process, service,..). *)

module Process : sig

  type program = string
  type arguments = string list
  type pid = int
  type exit_code = int
  type signal_name = string
  type mrproper = unit -> unit
  type running_state = Running | Suspended

  type options
  type tuning = unit -> options
  val make_options :
    ?enrich:options ->
    ?stdin:Endpoint.Source.t ->
    ?stdout:Endpoint.Sink.t ->
    ?stderr:Endpoint.Sink.t ->
    ?pseudo:string ->
    unit -> options

  type state =
    | Planned    of tuning * program * arguments
    | Started    of pid * mrproper * running_state
    | Terminated of pid * (signal_name, exit_code) Either.t

  val state_equality : state -> state -> bool

  val is_planned       : state -> bool
  val is_started       : state -> bool
  val is_suspended     : state -> bool
  val is_running       : state -> bool
  val is_terminated    : state -> bool

  type t = state Cortex.t
  type u = state Cortex.Open.t


  val plan :
    ?tuning:(unit -> options) ->
    program -> arguments -> t

  module Open :
    sig
      val plan :
	?tuning:(unit -> options) ->
	program -> arguments -> u
    end

  val start     : t -> state * bool
  val suspend   : ?nohang:unit -> t -> state * bool
  val resume    : ?nohang:unit -> t -> state * bool
  val terminate : ?nohang:unit -> t -> state * bool

  class c :
    ?tuning:(unit -> options) ->
    program ->
    arguments ->
    object
      inherit [state] Cortex.to_object_with_private_interface
      method start : unit -> state * bool
      method suspend : ?nohang:unit -> unit -> state * bool
      method resume : ?nohang:unit -> unit -> state * bool
      method terminate : ?nohang:unit -> unit -> state * bool
    end

end

module Service : sig

  type t = (Process.state option * Process.t) Cortex.t

  val plan :
    ?tuning:Process.tuning ->
    Process.program -> Process.arguments -> t

  val start           : t -> Process.state * bool
  val previous_status : t -> Process.state option
  val status          : t -> Process.state
  val suspend         : t -> Process.state * bool
  val resume          : ?nohang:unit -> t -> Process.state * bool
  val stop            : ?nohang:unit -> t -> Process.state * bool
  val restart         : t -> Process.state * bool

  class c :
    ?tuning:Process.tuning ->
    Process.program ->
    Process.arguments ->
    object
      inherit
        [Process.state option * Process.state Cortex.t]
           Cortex.to_object_with_private_interface
      method start           : unit -> Process.state * bool
      method previous_status : unit -> Process.state option
      method status          : unit -> Process.state
      method suspend         : unit -> Process.state * bool
      method resume          : ?nohang:unit -> unit -> Process.state * bool
      method stop            : ?nohang:unit -> unit -> Process.state * bool
      method restart         : unit -> Process.state * bool
    end

end
