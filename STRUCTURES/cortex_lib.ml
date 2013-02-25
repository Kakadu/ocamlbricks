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

(* Do not remove the following comment: it's an ocamldoc workaround. *)
(** *)
   
type program = string 
type arguments = string list
type pseudo = string 

module Process = struct

  type pid = int
  type exit_code = int
  type signal_name = string
  
  type running_state =
    | Running
    | Suspended

  type mrproper = unit -> unit (* to close channels *)

  type state =
  | Planned    of Endpoint.Source.t * Endpoint.Sink.t * Endpoint.Sink.t * (pseudo option) * program * arguments
  | Started    of pid * mrproper * running_state
  | Terminated of pid * (signal_name, exit_code) Either.t

  (* Mrproper is a pain, the standard equality will raise an exception (functional value),
     so it must be redefined: *)
  let state_equality x y =
    match (x,y) with
    | Started (p,_,r), Started (p',_,r') -> (p=p') && (r=r')
    | Started (_,_,_), _ | _, Started (_,_,_) -> false
    | x,y -> x=y

  let is_planned = function
    | Planned (_,_,_,_,_,_) -> true
    | _ -> false

  let is_started = function
    | Started (_,_,_) -> true
    | _ -> false

  let is_suspended = function
    | Started (_,_, Suspended) -> true
    | _ -> false

  let is_not_suspended = function
    | Started (_,_, Suspended) -> false
    | _ -> true

  let is_running = function
    | Started (_,_, Running) -> true
    | _ -> false

  let is_terminated = function
    | Terminated (_,_) -> true
    | _ -> false

  type t = state Cortex.t
  type u = state Cortex.Open.t

  let plan
    ?(stdin=Endpoint.Source.Empty)
    ?(stdout=Endpoint.Sink.Trash)
    ?(stderr=Endpoint.Sink.Trash)
    ?pseudo
    (program:string)
    (arguments:string list) : t
    =
    (* Set some transitions as forbidden (for instance, when terminal states are reached): *)
    let on_proposal s0 s1 =
      match (s0,s1) with
      | Terminated (_,_)   , _ -> s0
      | Started (_, _,_) , Planned (_,_,_,_,_,_) -> s0
      | _, _ -> s1
    in
    Cortex.return ~equality:state_equality ~on_proposal (Planned (stdin, stdout, stderr, pseudo, program, arguments))

  module Open = struct  
    let plan
      ?(stdin=Endpoint.Source.Empty)
      ?(stdout=Endpoint.Sink.Trash)
      ?(stderr=Endpoint.Sink.Trash)
      ?pseudo
      (program:string)
      (arguments:string list) : u
      =
      (* Set some transitions as forbidden (for instance, when terminal states are reached): *)
      let on_proposal s0 s1 =
	match (s0,s1) with
	| Terminated (_,_)   , _ -> s0
	| Started (_, _,_) , Planned (_,_,_,_,_,_) -> s0
	| _, _ -> s1
      in
      Cortex.Open.return ~equality:state_equality ~on_proposal (Planned (stdin, stdout, stderr, pseudo, program, arguments))
   end (* module Open *)
    
  (* Is a cortex evaluation, so it propose a transition that may be accepted or not,
     as may be observable by the caller in the result: *)
  let start t : (state * bool) =
    let transition = function
    | Planned (stdin, stdout, stderr, pseudo, program, arguments) ->
	let (stdin,  stdin_must_be_closed ) = Endpoint.Source.to_file_descr stdin in
	let (stdout, stdout_must_be_closed) = Endpoint.Sink.to_file_descr stdout  in
	let (stderr, stderr_must_be_closed) = Endpoint.Sink.to_file_descr stderr  in
	let name = match pseudo with None -> program | Some name -> name in
	let argv = (Array.of_list (name :: arguments)) in
	(* Channels' treatment: *)
        let mrproper () =
          begin
	    (if  stdin_must_be_closed then try Unix.close stdin with _ -> ());
	    (if stdout_must_be_closed then try Unix.close stdout with _ -> ());
	    (if stderr_must_be_closed then try Unix.close stderr with _ -> ());
	  end
	in
        let pid = Unix.create_process program argv stdin stdout stderr in
	Started (pid, mrproper, Running)
    | state -> state
    in (* end of transition() *)
    (* main of start() *)
    let (state', changed) = Cortex.move t transition in
    let () =
      if not changed then () else
      match state' with
      | Started (pid, mrproper, Running) ->
	  let _thread =
	    ThreadExtra.waitpid_thread
	      ~perform_when_suspended:
	        (fun ~pid -> Cortex.set t (Started (pid, mrproper, Suspended)))
	      ~perform_when_resumed:
	        (fun ~pid -> Cortex.set t (Started (pid, mrproper, Running)))
	      ~after_waiting:
		(fun ~pid status ->
		    let () = mrproper () in
		    let exiting_info =
		      match status with
		      | Unix.WSIGNALED signal -> Either.Left (SysExtra.name_of_signal signal)
		      | Unix.WEXITED code     -> Either.Right code
		      | _ -> assert false
		    in
		    Cortex.set t (Terminated (pid, exiting_info)))
	      ()
	      ~pid
	  in
	  ()
      | _ -> ()
    in
    (state', changed)

  let suspend ?nohang t : (state * bool) =
    let transition = function
    | Started (pid,_, Running) as state -> (Unix.kill pid Sys.sigstop; state)
    | state -> state
    in
    let (state, changed) = Cortex.move t transition in
    (* Now wait until the pause will be observed: *)
    let (state, changed) =
      match (is_running state) && (nohang = None) with
      | true  -> (Cortex.get ~guard:is_suspended t, true)
      | false -> (state, changed)
    in
    (state, changed)

  let resume ?nohang t : (state * bool) =
    let transition = function
    | Started (pid,_, Suspended) as state -> (Unix.kill pid Sys.sigcont; state)
    | state -> state
    in
    let (state, changed) = Cortex.move t transition in
    (* Now wait until the pause will be observed: *)
    let (state, changed) =
      match (is_suspended state) && (nohang = None) with
      | true  -> (Cortex.get ~guard:is_not_suspended t, true)
      | false -> (state, changed)
    in
    (state, changed)

  let rec terminate ?nohang ?sigkill t : (state * bool) =
    let term =
      if sigkill = Some () then Sys.sigkill else Sys.sigterm
    in
    let transition = function
    | Started (pid,_, Running) as state   -> (Unix.kill pid term; state)
    | Started (pid,_, Suspended) as state -> (List.iter (Unix.kill pid) [term; Sys.sigcont]; state)
    | state -> state
    in
    let (state, changed) = Cortex.move t transition in
    let () =
      if sigkill = None
       then ignore (Thread.create (fun () -> Thread.delay 0.5; terminate ~sigkill:() t) ())
       else ()
    in
    (* Now wait until the pause will be observed: *)
    let (state, changed) =
      match (is_started state) && (nohang = None) with
      | true  -> (Cortex.get ~guard:is_terminated t, true)
      | false -> (state, changed)
    in
    (state, changed)

  (* Redefinition: *)
  let terminate ?nohang t = terminate ?nohang ?sigkill:None t

end (* module Process *)


module Service = struct

  type t = ((Process.state option) * Process.t) Cortex.t
  
  let plan ?stdin ?stdout ?stderr ?pseudo program arguments : t =
    let creator ?previous () =
      Process.Open.plan ?stdin ?stdout ?stderr ?pseudo program arguments 
    in
    let terminal = Process.is_terminated in
    Cortex.lifes ~creator ~terminal ()
    
  let start (t:t) : (Process.state * bool) =
    Cortex.apply t (fun (_,p) -> Process.start p)

  let status (t:t) : Process.state =
    Cortex.apply t (fun (_,p) -> Cortex.get p)

  let previous_status (t:t) : Process.state option =
    Cortex.apply t (fun (s,_) -> s)

  let status (t:t) : Process.state =
    Cortex.apply t (fun (_,p) -> Cortex.get p)
  
  let stop ?nohang (t:t) : (Process.state * bool) =
    Cortex.apply t (fun (_,p) -> Process.terminate ?nohang p)

  let suspend (t:t) : (Process.state * bool) =
    Cortex.apply t (fun (_,p) -> Process.suspend p)

  let resume ?nohang (t:t) : (Process.state * bool) =
    Cortex.apply t (fun (_,p) -> Process.resume ?nohang p)

  (* Supposing recursive mutexes here (start t) in the critical section: *)
  let restart ?nohang (t:t) : (Process.state * bool) =
    Cortex.apply t 
      (fun (_,p) -> 
         let (_, changed) as stop_result = Process.terminate ?nohang p in
         if not changed then stop_result else start t)

(*  (* Without recursive mutexes we can break the critical section but it's
     not the same because another thread may start the service... *) 
  let restart (t:t) : (Process.state * bool) =
    let (_, changed) as stop_result = stop t in
    if not changed then stop_result else
    start t*)
         

end (* module Service *)
