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
   
(* Basic common types: *)
type program = string 
type arguments = string list
type pseudo = string 

module Process : sig
  
  type pid = int
  type exit_code = int
  type signal_name = string
  type mrproper = unit -> unit
  type running_state = Running | Suspended
  
  type state =
    | Planned    of Endpoint.Source.t * Endpoint.Sink.t * Endpoint.Sink.t * pseudo option * program * arguments
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
    ?stdin:Endpoint.Source.t ->
    ?stdout:Endpoint.Sink.t ->
    ?stderr:Endpoint.Sink.t -> 
    ?pseudo:pseudo -> 
    program -> arguments -> t
  
  module Open :
    sig
      val plan :
	?stdin:Endpoint.Source.t ->
	?stdout:Endpoint.Sink.t ->
	?stderr:Endpoint.Sink.t ->
	?pseudo:string -> 
	program -> arguments -> u
    end
  
  val start     : t -> state * bool
  val suspend   : ?nohang:'a -> t -> state * bool
  val resume    : ?nohang:'a -> t -> state * bool
  val terminate : ?nohang:'a -> t -> state * bool
end

module Service : sig

  type t = (Process.state option * Process.t) Cortex.t
  
  val plan :
    ?stdin:Endpoint.Source.t ->
    ?stdout:Endpoint.Sink.t ->
    ?stderr:Endpoint.Sink.t -> 
    ?pseudo:pseudo -> 
    program -> arguments -> t
  
  val start           : t -> Process.state * bool
  val previous_status : t -> Process.state option
  val status          : t -> Process.state
  val suspend         : t -> Process.state * bool
  val resume          : ?nohang:'a -> t -> Process.state * bool
  val stop            : ?nohang:'a -> t -> Process.state * bool
  val restart         : ?nohang:'a -> t -> Process.state * bool

end
