(* This file is part of our reusable OCaml BRICKS library
   Copyright (C) 2007  Jean-Vincent Loddo

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


(*module Fold :
  sig
    val commacat         : string list -> string
    val semicolon        : string list -> string
    val nospacecommacat  : string list -> string
    val nospacesemicolon : string list -> string
    val dotcat           : string list -> string
    val newlinecat       : string list -> string
    val blankcat         : string list -> string
    val slashcat         : string list -> string
  end*)


(*(** Examples of applications of [big] constructor
    in conjonction with the [merge] function. *)
module Fold = struct

 (** Merge a string list with the separator [" , "]. *)
 let commacat = big (merge " , ");;

 (** Merge a string list with the separator ["; "]. *)
 let semicolon = big (merge "; ");;

 (** Merge a string list with the separator [","]. *)
 let nospacecommacat = big (merge ",");;

 (** Merge a string list with the separator [";"]. *)
 let nospacesemicolon = big (merge ";");;

 (** Merge a string list with the separator ["."]. *)
 let dotcat = big (merge ".");;

 (** Merge a string list with the separator ["\n"]. *)
 let newlinecat = big (merge "\n");;

 (** Merge a string list with the separator [" "]. *)
 let blankcat = big (merge " ");;

 (** Merge a string list with the separator ["/"]. *)
 let slashcat = big (merge "/");;

end (* module Fold *)*)


let try_finalize f x finally y =
      let res = try f x with exn -> finally y; raise exn in
      finally y;
      res
;;




(** Additionnal tools for list manipulations *)
module LList = struct


 (** {2 combine 2-8} *)

 let combine2 = List.combine ;;

 let rec combine5 l1 l2 l3 l4 l5 = match (l1,l2,l3,l4,l5) with
  | []    , []    , []    , []    , []     -> []
  | x1::r1, x2::r2, x3::r3, x4::r4, x5::r5 -> (x1,x2,x3,x4,x5)::(combine5 r1 r2 r3 r4 r5)
  | _ -> raise (Invalid_argument "combine5")
 ;;

 let rec combine6 l1 l2 l3 l4 l5 l6 = match (l1,l2,l3,l4,l5,l6) with
  | []    , []    , []    , []    , []     , []     -> []
  | x1::r1, x2::r2, x3::r3, x4::r4, x5::r5 , x6::r6 -> (x1,x2,x3,x4,x5,x6)::(combine6 r1 r2 r3 r4 r5 r6)
  | _ -> raise (Invalid_argument "combine6")
 ;;

 let rec combine7 l1 l2 l3 l4 l5 l6 l7 = match (l1,l2,l3,l4,l5,l6,l7) with
  | []    , []    , []    , []    , []     , []     , []     -> []
  | x1::r1, x2::r2, x3::r3, x4::r4, x5::r5 , x6::r6 , x7::r7 -> (x1,x2,x3,x4,x5,x6,x7)::(combine7 r1 r2 r3 r4 r5 r6 r7)
  | _ -> raise (Invalid_argument "combine7")
 ;;

 let rec combine8 l1 l2 l3 l4 l5 l6 l7 l8 = match (l1,l2,l3,l4,l5,l6,l7,l8) with
  | []    , []    , []    , []    , []     , []     , []     , []     -> []
  | x1::r1, x2::r2, x3::r3, x4::r4, x5::r5 , x6::r6 , x7::r7 , x8::r8 -> (x1,x2,x3,x4,x5,x6,x7,x8)::(combine8 r1 r2 r3 r4 r5 r6 r7 r8)
  | _ -> raise (Invalid_argument "combine8")
 ;;

 (** {2 split 2-8} *)

 let split2 = List.split ;;

 let rec split3 l = match l with
 | [] -> ([],[],[])
 | (x1,x2,x3)::r -> let (s1,s2,s3) = (split3 r) in (x1::s1,x2::s2,x3::s3)
 ;;

 let rec split4 l = match l with
 | [] -> ([],[],[],[])
 | (x1,x2,x3,x4)::r -> let (s1,s2,s3,s4) = (split4 r) in (x1::s1,x2::s2,x3::s3,x4::s4)
 ;;

 let rec split5 l = match l with
 | [] -> ([],[],[],[],[])
 | (x1,x2,x3,x4,x5)::r -> let (s1,s2,s3,s4,s5) = (split5 r) in (x1::s1,x2::s2,x3::s3,x4::s4,x5::s5)
 ;;

 let rec split6 l = match l with
 | [] -> ([],[],[],[],[],[])
 | (x1,x2,x3,x4,x5,x6)::r -> let (s1,s2,s3,s4,s5,s6) = (split6 r) in (x1::s1,x2::s2,x3::s3,x4::s4,x5::s5,x6::s6)
 ;;

 let rec split7 l = match l with
 | [] -> ([],[],[],[],[],[],[])
 | (x1,x2,x3,x4,x5,x6,x7)::r -> let (s1,s2,s3,s4,s5,s6,s7) = (split7 r) in (x1::s1,x2::s2,x3::s3,x4::s4,x5::s5,x6::s6,x7::s7)
 ;;

 let rec split8 l = match l with
 | [] -> ([],[],[],[],[],[],[],[])
 | (x1,x2,x3,x4,x5,x6,x7,x8)::r -> let (s1,s2,s3,s4,s5,s6,s7,s8) = (split8 r) in (x1::s1,x2::s2,x3::s3,x4::s4,x5::s5,x6::s6,x7::s7,x8::s8)
 ;;

end ;;

let g x = let _ = Thread.delay (float_of_int x) in ignore (Sys.command ("touch /tmp/pluto."^(string_of_int x))) ;;
let xs = Array.init 31 (fun i -> 34+i);;  
let ts = Array.map (fun x -> ThreadExtra.create ~killable:() g x) xs ;;
Array.iteri (fun i t -> Thread.delay 0.1; if i mod 3 = 0 then () else ignore (ThreadExtra.kill t)) ts ;;
let k = ThreadExtra.killer ts.(2) ;;
ThreadExtra.fork (fun () -> Thread.delay 1.; k (); Thread.delay 1.;) () ;;

Log.printf "Peeking: %s\n" (Option.string_of (fun x-> string_of_int (Obj.magic x)) (ch#peek ~at_least:8 ()));

let (thrd4, addr4, port4) as r4 = inet4_server ?max_pending_requests ?killable ?tutor_behaviour ?no_fork ?ipv4 ?port server_fun in
  Log.printf "stream_inet_server: inet4 thread started (%d)\n" (Thread.id thrd4);
  let return_raising e =
    Log.print_exn ~prefix:"stream_inet_server: I cannot start both servers because of: " e;
    (* Try to kill thrd4 after having waited 1 second (thrd4 shoud have the time tu register its killing thunk),
       but do this in another thread, in order to return immediately: *)
    ThreadExtra.delayed_kill 1. thrd4;
    raise e
  in
  let (thrd6, addr6, port6) as r6 =
    try
      inet6_server ?max_pending_requests ?killable ?tutor_behaviour ?no_fork ?ipv6 ~port:port4 server_fun
    with
    | Binding e when port=None ->
	(try
	   inet6_server ?max_pending_requests ?killable ?tutor_behaviour ?no_fork ?ipv6 ?port server_fun
	 with e -> return_raising e)
    | e -> return_raising e
  in
  Log.printf "stream_inet_server: inet6 thread started (%d)\n" (Thread.id thrd6);
  (r4,r6)


let log_signal_receptions () =
  let simulated_handler_of_default_action = function
  | Ign  -> Some ignore
  | Term -> Some (fun i -> exit (128+(int_of_signal i)))
  | Core -> Some (fun i -> Sys.set_signal i Sys.Signal_default; Unix.kill (Unix.getpid ()) i)
  | _ -> None
  in
  let wrapper ~signo ~name ~descr ~current_handler =
    Sys.Signal_handle
      (fun i ->
	 Log.printf "Received signal %d (%s): %s\n" signo name descr;
         current_handler i)
  in
  iter_on_signals
    (fun i behavior ->
       let signo = int_of_signal i in
       let name  = string_of_signal signo in
       let (_, action, descr) = description_of_signal name in
       match behavior with
       | Sys.Signal_handle current_handler -> Sys.set_signal i (wrapper ~signo ~name ~descr ~current_handler)
       | Sys.Signal_ignore                 -> Sys.set_signal i (wrapper ~signo ~name ~descr ~current_handler:ignore)
       | Sys.Signal_default ->
           (match simulated_handler_of_default_action action with
           | None -> ()
           | Some current_handler -> Sys.set_signal i (wrapper ~signo ~name ~descr ~current_handler)
           )
       )


let delay time =
  let t =
    Thread.create
      (fun () ->
         let _ = Thread.sigmask Unix.SIG_BLOCK [17,23,26,28] in
         Unix.select [] [] [] time)
      ()
      in
  let rec loop () =
    try
      Thread.join t
    with
    | Unix.Unix_error (Unix.EINTR, _, _) -> loop ()
  in loop ()



let of_string ~(a:string -> 'a) ~(b:string -> 'b) x =
  let ic = Scanf.Scanning.from_string x in
  let a' scanbuf = Scanf.bscanf scanbuf "%s" a in
  let b' scanbuf = Scanf.bscanf scanbuf "%s" b in
  let rec self ic =
    try Scanf.bscanf ic "Left %r"       a' (fun v -> Left v) with _ ->
    try Scanf.bscanf ic "Right %r"      b' (fun v -> Right v) with _ ->
    Scanf.bscanf ic "( %r )" self (fun v -> v)
  in self ic

let of_Genlex_token_stream  ~(a:Genlex.token Stream.t -> 'a) ~(b:Genlex.token Stream.t -> 'b) x =
  

val interactive_stream_unix_client :
  ?max_input_size:int ->
  socketfile:string ->
  unit -> (exn,'a) Either.t

let interactive_stream_unix_client ?max_input_size ~socketfile () =
  let rec protocol (ch:stream_channel) =
    try
      let cmd = read_line () in
      if cmd = "quit" then () else
      ch#output_line cmd;
      Thread.delay 0.5;
      let answer = ch#receive () in
      Printf.printf "%s" answer;
      protocol ch
    with _ -> ()
  in
  stream_unix_client ?max_input_size ~socketfile ~protocol ()

let ask_vde_switch_for_current_numports ~socketfile () =
  let protocol (ch:stream_channel) =
    ch#output_line "port/showinfo";
    let rec loop () =
      let answer = ch#input_line () in
      match Option.apply_or_catch (Scanf.sscanf answer "Numports=%d") (fun i -> i) with
      | None -> loop ()
      | Some i -> i
    in
    loop ()
  in
  stream_unix_client ~socketfile ~protocol ()

let wait_vde_switch_until_numports_are_allocated ~numports ~socketfile () =
  let rec protocol (ch:stream_channel) =
    ch#output_line "port/showinfo";
    let rec loop () =
      let answer = ch#input_line () in
      match Option.apply_or_catch (Scanf.sscanf answer "Numports=%d") (fun i -> i) with
      | None -> loop ()
      | Some i -> if i>=numports then i else (Thread.delay 0.2; (protocol ch))
    in
    loop ()
  in
  stream_unix_client ~socketfile ~protocol ()


