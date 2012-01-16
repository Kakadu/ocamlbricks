(* This file is part of ocamlbricks
   Copyright (C) 2011, 2012 Jean-Vincent Loddo

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


module Log = Ocamlbricks_log

exception Accepting of exn
exception Connecting of exn
exception Receiving of exn
exception Sending   of exn
exception Closing   of exn
exception Binding   of exn
  
let string_of_sockaddr = function
  | Unix.ADDR_UNIX x -> x
  | Unix.ADDR_INET (inet_addr, port) ->
      Printf.sprintf "%s:%d" (Unix.string_of_inet_addr inet_addr) port

(** Extract the name of the associated socket file from a unix domain sockaddr.
   Raises [Invalid_argument] if the sockaddr is not in the unix domain. *)
let socketfile_of_sockaddr = function
  | Unix.ADDR_UNIX x -> x
  | _ -> invalid_arg "Network.socketfile_of_sockaddr"

(** Extract the inet_addr and port from a inet domain sockaddr.
    Raises [Invalid_argument] if the sockaddr is not in the inet domain. *)
let inet_addr_and_port_of_sockaddr = function
  | Unix.ADDR_INET (inet_addr, port) -> (inet_addr, port)
  | _ -> invalid_arg "Network.inet_addr_of_sockaddr"

let domain_of_inet_addr x =
  Unix.domain_of_sockaddr (Unix.ADDR_INET (x, 0))

let string_of_domain = function
| Unix.PF_UNIX  -> "Unix domain"
| Unix.PF_INET  -> "Internet domain (IPv4)"
| Unix.PF_INET6 -> "Internet domain (IPv6)"

(* Inspired by the homonymous function in the standard library unix.ml *)
let rec accept_non_intr s =
  try Unix.accept s
  with
  | Unix.Unix_error (Unix.EINTR, _, _) -> accept_non_intr s
  | e -> raise (Accepting e)

(* Unix.bind wrapper: raises the exception Binding if something goes wrong: *)
let bind socket sockaddr =
  try
    Unix.bind socket sockaddr
  with e ->
    let (inet_addr, port) = inet_addr_and_port_of_sockaddr sockaddr in
    let domain = string_of_domain (domain_of_inet_addr inet_addr) in
    Log.print_exn ~prefix:(Printf.sprintf "binding socket to %s address %s: " domain (string_of_sockaddr sockaddr)) e;
    raise (Binding e)

(* fix Unix.IPV6_ONLY if needed *)
let fix_IPV6_ONLY_if_needed ~domain fd =
  if domain <> Unix.PF_INET6 then () else
  let ipv6_only = Unix.getsockopt fd Unix.IPV6_ONLY in
  (if not ipv6_only then
    Log.printf "Fixing option Unix.IPV6_ONLY to true\n";
    Unix.setsockopt fd Unix.IPV6_ONLY true);
  ()

(* Generic function able to establish a server on a sockaddr. *)
let server ?(max_pending_requests=5) ?seqpacket ?tutor_behaviour ?no_fork server_fun sockaddr =
  let socket_type =
    match seqpacket with
    | None    -> Unix.SOCK_STREAM
    | Some () -> Unix.SOCK_SEQPACKET (* implies domain = Unix.ADDR_UNIX *)
  in
  let domain = Unix.domain_of_sockaddr sockaddr in
  let listen_socket = Unix.socket domain socket_type 0 in
  (* listen_socket initialization: *)
  let assigned_port =
    Unix.setsockopt listen_socket Unix.SO_REUSEADDR true;
    fix_IPV6_ONLY_if_needed ~domain listen_socket;
    bind listen_socket sockaddr;
    Unix.listen listen_socket max_pending_requests;
    (* The assigned port will be interesting for the caller only if the port number 
       provided with ~sockaddr has been set to 0 (in order to ask the kernel to choose
       itself): *)
    match Unix.getsockname listen_socket with
    | Unix.ADDR_INET (_, assigned_port) -> Some assigned_port
    | Unix.ADDR_UNIX socketfile ->
        let () = Unix.chmod socketfile 0o777 in
        None
  in
  let listen_socket_as_string =
    string_of_sockaddr (Unix.getsockname listen_socket)
  in
  let notify_after_accept_and_get_sockaddr0 ~connexion_no ~service_socket =
    incr connexion_no;
    let sockaddr0 = string_of_sockaddr (Unix.getsockname service_socket) in
    let sockaddr1 = string_of_sockaddr (Unix.getpeername service_socket) in
    Log.printf "Accepted connection #%d on %s from %s\n" !connexion_no sockaddr0 sockaddr1;
    sockaddr0
  in
  let exit_code_and_final_notification ~connexion_no ~sockaddr0 ~result =
    match result with
    | Either.Right () ->
	let () = Log.printf "Protocol completed (connection #%d on %s). Exiting.\n" !connexion_no sockaddr0
	in 0
    | Either.Left _ ->
	let () = Log.printf "Protocol interrupted (connection #%d on %s). Exiting\n" !connexion_no sockaddr0
	in 1
  in
  let process_forking_loop () =
    let connexion_no = ref 0 in
    let tutor = ThreadExtra.tutor ?behaviour:tutor_behaviour () in
    while true do
      Log.printf "Waiting for connection on %s\n" listen_socket_as_string;
      let (service_socket, _) = accept_non_intr listen_socket in
      incr connexion_no;
      let sockaddr0 = notify_after_accept_and_get_sockaddr0 ~connexion_no ~service_socket in
      match Unix.fork () with
      |	0 ->
          (* The child here: *)
          begin
            Log.printf "Process (fork) created for connection #%d on %s\n" !connexion_no sockaddr0;
            (* SysExtra.log_signal_reception ~except:[26] (); *)
	    Unix.close listen_socket;
	    (try Unix.set_close_on_exec service_socket with Invalid_argument _ -> ());
	    let result = server_fun service_socket in
	    let exit_code = exit_code_and_final_notification ~connexion_no ~sockaddr0 ~result in
	    exit exit_code
	  end
      | child_pid ->
          (* The father here creates a process-tutor thread per child: *)
          begin
            Unix.close service_socket;
            ignore (tutor child_pid)
          end
    done
  in
  let thread_forking_loop () =
    let connexion_no = ref 0 in
    while true do
      let (service_socket, _) = accept_non_intr listen_socket in
      let sockaddr0 = notify_after_accept_and_get_sockaddr0 ~connexion_no ~service_socket in
      let server_fun s =
        Log.printf "Thread created for connection #%d on %s\n" !connexion_no sockaddr0;
        let result = server_fun s in
        let _unused_exit_code = exit_code_and_final_notification ~connexion_no ~sockaddr0 ~result in
	Thread.exit ()
      in
      ignore (ThreadExtra.create server_fun service_socket);
    done
  in
  let forking_loop () =
    (* Provide to the other threads a mean to kill this forking_loop: *)
    let () =
      let shutdown () = Unix.shutdown listen_socket Unix.SHUTDOWN_RECEIVE in
      ThreadExtra.set_killable_with_thunk (fun () -> shutdown ())
    in
    (* listen_socket finalization: *)
    let () =
      match sockaddr with
      | Unix.ADDR_UNIX filename ->
	  ThreadExtra.at_exit (fun () -> Unix.unlink filename)
      | _ -> ()
    in
    (* process or thread switching: *)
    match no_fork with
    | None    -> process_forking_loop ()
    | Some () -> thread_forking_loop ()
  in
  let server_thread = ThreadExtra.create forking_loop () in
  (server_thread, assigned_port)
  

let socketname_in_a_fresh_made_directory ?temp_dir ?prefix ?suffix ?(perm=0o777) basename =
  let prefix = match prefix with
    | Some x -> x
    | None   ->
        Printf.sprintf ".%s-%d.%d-sockets-"
          (Filename.basename (Sys.executable_name))
          (Unix.getpid ())
          (Thread.id (Thread.self ()))
  in
  let fresh_made_dir = FilenameExtra.temp_dir ?temp_dir ~prefix ?suffix ~perm () in
  let result = (String.concat "/" [fresh_made_dir; basename]) in
  let () = ThreadExtra.at_exit (fun () -> Unix.rmdir fresh_made_dir) in
  let () = ThreadExtra.at_exit (fun () -> Unix.unlink result) in
  result

let fresh_socketname ?temp_dir ?prefix ?(suffix="") () =
  let prefix = match prefix with
    | Some x -> x
    | None   ->
        Printf.sprintf ".%s-%d.%d-socket-"
          (Filename.basename (Sys.executable_name))
          (Unix.getpid ())
          (Thread.id (Thread.self ()))
  in
  let result = Filename.temp_file ?temp_dir prefix suffix in
  let () = Unix.unlink result in
  let () = ThreadExtra.at_exit (fun () -> Unix.unlink result) in
  result

let unix_server ?max_pending_requests ?seqpacket ?tutor_behaviour ?no_fork ?socketfile server_fun =
  let socketfile = Option.extract_or_force socketfile (lazy (socketname_in_a_fresh_made_directory "ctrl")) in
  let sockaddr = Unix.ADDR_UNIX socketfile in
  let (server_thread, _) = server ?max_pending_requests ?seqpacket ?tutor_behaviour ?no_fork server_fun sockaddr in
  (server_thread, socketfile)

let inet4_server ?max_pending_requests ?tutor_behaviour ?no_fork ?ipv4 ?(port=0) server_fun =
  let ipv4 = match ipv4 with
    | Some x -> Unix.inet_addr_of_string x
    | None   -> Unix.inet_addr_any
  in
  let sockaddr = Unix.ADDR_INET (ipv4, port) in
  let (server_thread, assigned_port) = 
    server ?max_pending_requests ?tutor_behaviour ?no_fork server_fun sockaddr
  in
  let assigned_port = match assigned_port with
  | Some x -> x 
  | None -> assert false
  in
  (server_thread, (Unix.string_of_inet_addr ipv4), assigned_port)

let inet6_server ?max_pending_requests ?tutor_behaviour ?no_fork ?ipv6 ?(port=0) server_fun =
  let ipv6 = match ipv6 with
    | Some x -> Unix.inet_addr_of_string x
    | None   -> Unix.inet6_addr_any
  in
  let sockaddr = Unix.ADDR_INET (ipv6, port) in
  let (server_thread, assigned_port) = 
    server ?max_pending_requests ?tutor_behaviour ?no_fork server_fun sockaddr
  in
  let assigned_port = match assigned_port with
  | Some x -> x 
  | None -> assert false
  in
  (server_thread, (Unix.string_of_inet_addr ipv6), assigned_port)

(* Dual stack inet4 and inet6: *)
let inet_server ?max_pending_requests ?tutor_behaviour ?no_fork ?ipv4 ?ipv6 ?port server_fun =
  let (thrd4, addr4, port4) as r4 = inet4_server ?max_pending_requests ?tutor_behaviour ?no_fork ?ipv4 ?port server_fun in
  Log.printf "dual stack server: inet4 thread started (%d)\n" (Thread.id thrd4);
  let return_raising e =
    Log.print_exn ~prefix:"dual stack server: I cannot start both servers because of: " e;
    (* Try to kill thrd4 after having waited 1 second (thrd4 shoud have the time tu register its killing thunk),
       but do this in another thread, in order to return immediately: *)
    ThreadExtra.delayed_kill 1. thrd4;
    raise e
  in
  let (thrd6, addr6, port6) as r6 =
    try
      inet6_server ?max_pending_requests ?tutor_behaviour ?no_fork ?ipv6 ~port:port4 server_fun
    with
    | Binding e when port=None ->
	(try
	   inet6_server ?max_pending_requests ?tutor_behaviour ?no_fork ?ipv6 ?port server_fun
	 with e -> return_raising e)
    | e -> return_raising e
  in
  Log.printf "dual stack server: inet6 thread started (%d)\n" (Thread.id thrd6);
  (r4,r6)

(* fix Unix.SO_RCVBUF if needed *)
let fix_SO_RCVBUF_if_needed ~max_input_size fd =
  let recv_buffer_size = Unix.getsockopt_int fd Unix.SO_RCVBUF in
  (if max_input_size > recv_buffer_size then
    Log.printf "Fixing option Unix.SO_RCVBUF to the value %d\n" max_input_size;
    Unix.setsockopt_int fd Unix.SO_RCVBUF max_input_size);
  ()


class common_low_level_methods_on_socket fd =
 object
  method get_send_buffer_size   = Unix.getsockopt_int fd Unix.SO_SNDBUF
  method set_send_buffer_size x = Unix.setsockopt_int fd Unix.SO_SNDBUF x

  method get_recv_buffer_size   = Unix.getsockopt_int fd Unix.SO_RCVBUF
  method set_recv_buffer_size x = Unix.setsockopt_int fd Unix.SO_RCVBUF x

  method get_close_linger   = Unix.getsockopt_optint fd Unix.SO_LINGER
  method set_close_linger x = Unix.setsockopt_optint fd Unix.SO_LINGER x
 end

(* High-level representation of the structure available, after a connection, to both endpoints.
   The max_input_size is set by default to 1514 (Ethernet: 1514=1526-12 (8 preamble and 4 CRC)) *)
class stream_or_seqpacket_bidirectional_channel ?(max_input_size=1514) ?seqpacket fd =
  let () = fix_SO_RCVBUF_if_needed ~max_input_size fd in
  object
  inherit common_low_level_methods_on_socket fd

  val input_buffer = String.create max_input_size
  val max_input_size = max_input_size

  method shutdown ?receive ?send () =
    try
      let shutdown_command =
	match receive, send with
	| None, None | Some (), None -> Unix.SHUTDOWN_RECEIVE
	| None, Some () -> Unix.SHUTDOWN_SEND
	| Some (), Some () -> Unix.SHUTDOWN_ALL
      in
      Unix.shutdown fd shutdown_command
    with e ->
      Log.print_exn ~prefix:"channel#shutdown: " e;
      raise (Closing e)

  method sockaddr0 = Unix.getsockname fd
  method sockaddr1 = Unix.getpeername fd

end (* class stream_or_seqpacket_bidirectional_channel *)

class stream_channel ?max_input_size fd =
  let in_channel  = Unix.in_channel_of_descr  fd in
  let out_channel = Unix.out_channel_of_descr fd in
  let raise_but_also_log_it ?sending caller e =
    let prefix = Printf.sprintf "stream_channel#%s: " caller in
    let () = Log.print_exn ~prefix e in
    if sending=None then raise (Receiving e) else raise (Sending e)
  in
  let tutor0 f x caller =
    try
      f x
    with e -> raise_but_also_log_it caller e
  in
  let tutor1 f x y caller =
    try
      f x y;
      flush x
    with e -> raise_but_also_log_it ~sending:() caller e
  in
  let return_of_at_least at_least =
    match at_least with
    | None -> fun y -> y
    | Some m ->
	let previous = Unix.getsockopt_int fd Unix.SO_RCVLOWAT in
	let () = Unix.setsockopt_int fd Unix.SO_RCVLOWAT m in
	fun y ->
	  (* restore the previous value and return: *)
	  let () = Unix.setsockopt_int fd Unix.SO_RCVLOWAT previous in
	  y
  in
  object
  inherit stream_or_seqpacket_bidirectional_channel ?max_input_size fd as super

  method receive ?at_least () : string =
    let return = return_of_at_least at_least in
    try
      let n = Unix.recv fd input_buffer 0 max_input_size [] in
      (if n=0 then failwith "received 0 bytes (peer terminated?)");
      return (String.sub input_buffer 0 n)
    with e ->
      Log.print_exn ~prefix:"stream_channel#receive: " e;
      let _ = return "" in
      raise (Receiving e)

  method peek ?(at_least=0) () : string option =
    try
      Unix.set_nonblock fd;
      let n = Unix.recv fd input_buffer 0 max_input_size [Unix.MSG_PEEK] in
      Unix.clear_nonblock fd;
      if n>=at_least
       then Some (String.sub input_buffer 0 n)
       else
         let () = if at_least>0 then Log.printf "stream_channel#peek: received %d bytes (expected at least %d)\n" n at_least in
         None
    with e ->
      Unix.clear_nonblock fd;
      Log.print_exn ~prefix:"stream_channel#peek: result is None because of exception: " e;
      None

  method send (x:string) : unit =
    let rec send_stream_loop x off len =
      if len=0 then () else
      let n = Unix.send fd x off len [] in
      if n = 0 then failwith "failed to send in a stream channel: no more than 0 bytes sent!" else
      if n<len then send_stream_loop x (off+n) (len-n) else
      ()
    in
    try
      send_stream_loop x 0 (String.length x)
    with e ->
      Log.print_exn ~prefix:"stream_channel#send: " e;
      raise (Sending e)

  method input_char       () : char   = tutor0 Pervasives.input_char in_channel "input_char"
  method input_line       () : string = tutor0 Pervasives.input_line in_channel "input_line"
  method input_byte       () : int    = tutor0 Pervasives.input_byte in_channel "input_byte"
  method input_binary_int () : int    = tutor0 Pervasives.input_binary_int in_channel "input_binary_int"
  method input_value         : 'a. unit -> 'a = fun () -> tutor0 Pervasives.input_value in_channel "input_value"

  method output_char   x = tutor1 Pervasives.output_char out_channel x "output_char"
  method output_line   x = tutor1 Pervasives.output_string out_channel (x^"\n") "output_line"
  method output_byte   x = tutor1 Pervasives.output_byte out_channel x "output_byte"
  method output_binary_int x = tutor1 Pervasives.output_binary_int out_channel x "output_binary_int"
  method output_value : 'a. 'a -> unit =
    fun x -> tutor1 Pervasives.output_value out_channel x "output_value"

  method get_send_wait_at_least   = Unix.getsockopt_int fd Unix.SO_SNDLOWAT
  method set_send_wait_at_least x = Unix.setsockopt_int fd Unix.SO_SNDLOWAT x

  method get_recv_wait_at_least   = Unix.getsockopt_int fd Unix.SO_RCVLOWAT
  method set_recv_wait_at_least x = Unix.setsockopt_int fd Unix.SO_RCVLOWAT x

end (* class stream_channel *)

(** Useful for writing polymorphic protocols that refer only to method #send and #receive: *)
let line_oriented_channel_of_stream_channel (ch:stream_channel) =
  object
    method receive = ch#input_line
    method send    = ch#output_line
    method peek () =
      match ch#peek ~at_least:1 () with
      | None -> None
      | Some s ->
          (try
            let i = String.index s '\n' in
            Some (String.sub s 0 i) (* do not include "\n" *)
           with Not_found -> None)
  end

class seqpacket_channel ?max_input_size fd =
  object
  inherit stream_or_seqpacket_bidirectional_channel ?max_input_size ~seqpacket:() fd

  method receive () : string =
    try
      let n = Unix.recv fd input_buffer 0 max_input_size [] in
      (if n=0 then failwith "received 0 bytes (peer terminated?)");
      String.sub input_buffer 0 n
    with e ->
      Log.print_exn ~prefix:"seqpacket_channel#receive: " e;
      raise (Receiving e)

  method peek () : string option =
    try
      Unix.set_nonblock fd;
      let n = Unix.recv fd input_buffer 0 max_input_size [Unix.MSG_PEEK] in
      Unix.clear_nonblock fd;
      if n>0 then Some (String.sub input_buffer 0 n) else None
    with e ->
      Unix.clear_nonblock fd;
      Log.print_exn ~prefix:"seqpacket_channel#peek: result is None because of exception: " e;
      None

  method send (x:string) : unit =
    try
      let len = String.length x in
      let n = Unix.send fd x 0 len [] in
      if n<len then
	failwith (Printf.sprintf "failed sending a seqpacket: no more than %d bytes sent!" n)
      else ()
    with e ->
      Log.print_exn ~prefix:"seqpacket_channel#send: " e;
      raise (Sending e)

end (* class seqpacket_channel *)

exception Unexpected_sender of string

(* Typically the client builds its socketfile (0), send it to the server through the stream channel, then
   receives its socketfile for output (1).  *)
class dgram_channel ?(max_input_size=1514) ~fd0 ~sockaddr1 () =
  let () = fix_SO_RCVBUF_if_needed ~max_input_size fd0 in
  let input_buffer = String.create max_input_size in
  let sockaddr0 = Unix.getsockname fd0 in
  object (self)
  inherit common_low_level_methods_on_socket fd0

  method receive () : string =
    try
      let (n, sockaddr) = Unix.recvfrom fd0 input_buffer 0 max_input_size [] in
      (if sockaddr <> sockaddr1 then raise (Unexpected_sender (string_of_sockaddr sockaddr)));
      String.sub input_buffer 0 n
    with e ->
      Log.print_exn ~prefix:"dgram_channel#receive: " e;
      raise (Receiving e)

  method peek () : string option =
    try  
      Unix.set_nonblock fd0;
      let (n, sockaddr) = Unix.recvfrom fd0 input_buffer 0 max_input_size [Unix.MSG_PEEK] in
      Unix.clear_nonblock fd0;
      (if sockaddr <> sockaddr1 then raise (Unexpected_sender (string_of_sockaddr sockaddr)));
      if n>0 then Some (String.sub input_buffer 0 n) else None
    with e ->
      Unix.clear_nonblock fd0;
      Log.print_exn ~prefix:"dgram_channel#peek: result is None because of exception: " e;
      None

  method send (x:string) : unit =
    try
      let len = String.length x in
      (* fd0 represents where I want to receive the answer: *)
      let n = Unix.sendto fd0 x 0 len [] sockaddr1 in
      if n<len then failwith (Printf.sprintf "dgram_channel#send: no more than %d bytes sent (instead of %d)" n len) else
      ()
    with e ->
      Log.print_exn ~prefix:"dgram_channel#send: " e;
      raise (Sending e)
    
  method shutdown ?receive ?send () =
    try
      let shutdown_command =
	match receive, send with
	| None, None | Some (), None -> Unix.SHUTDOWN_RECEIVE
	| None, Some () -> Unix.SHUTDOWN_SEND
	| Some (), Some () -> Unix.SHUTDOWN_ALL
      in
      (match shutdown_command with
      | Unix.SHUTDOWN_RECEIVE | Unix.SHUTDOWN_ALL ->
	  (try Unix.close fd0 with _ -> ());
	  (try Unix.unlink (socketfile_of_sockaddr sockaddr0) with _ -> ());
      | _ -> ()
      );
      (match shutdown_command with
      | Unix.SHUTDOWN_SEND | Unix.SHUTDOWN_ALL ->
	  (try Unix.unlink (socketfile_of_sockaddr sockaddr1) with _ -> ());
      | _ -> ()
      )
    with e ->
      Log.print_exn ~prefix:"dgram_channel#shutdown: " e;
      raise (Closing e)

  method sockaddr0 = sockaddr0
  method sockaddr1 = sockaddr1

  method chmod_sockaddr0 x =
    match sockaddr0 with
    | Unix.ADDR_UNIX socketfile -> Unix.chmod socketfile x
    | _ -> ()

  initializer
    self#chmod_sockaddr0 0o777

end (* class dgram_channel *)


let dgram_input_socketfile_of ?dgram_output_socketfile ~stream_socketfile () =
  let make_socket ~bind_to =
    let result = Unix.socket Unix.PF_UNIX Unix.SOCK_DGRAM 0 in
    let socketfile = bind_to in
    bind result (Unix.ADDR_UNIX socketfile);
    Log.printf "Unix datagram socket bound to %s\n" socketfile;
    result
  in
  let socketfile1 = dgram_output_socketfile in
  let socketfile0 =
    let temp_dir = Filename.dirname stream_socketfile in
    (* Example: 14219.0<===8173az2 *)
    let prefix = Printf.sprintf "%d.%d<===" (Unix.getpid ()) (Thread.id (Thread.self ())) in
    let create_name_from_socketfile () =
      (* Filename.temp_file add an hexadecimal string after the prefix: *)
      let result = Filename.temp_file ~temp_dir prefix "" in
      let () = Unix.unlink result in
      result
    in
    let make_the_symlink ~link_suffix ~target =
      let link_prefix = Printf.sprintf "%d.%d>===" (Unix.getpid ()) (Thread.id (Thread.self ())) in
      let link_name = Printf.sprintf "%s/%s%s" temp_dir link_prefix link_suffix in
      Unix.symlink target link_name;
      ThreadExtra.at_exit (fun () -> Unix.unlink link_name)
    in
    let try_to_create_name_from_socketfile1_generated_by_this_library () =
      let socketfile1 = Option.extract socketfile1 in
      (assert (temp_dir = Filename.dirname socketfile1));
      let basename1 = Filename.basename socketfile1 in
      let (process, thread, channel_tag) =
        Scanf.sscanf basename1 "%d.%d<===%s" (fun p t s -> (p,t,s))
      in
      (* Example: 14219.0<===8173az2===<14220.1 *)
      let candidate = Printf.sprintf "%s/%s%s===<%d.%d" temp_dir prefix channel_tag process thread in
      (assert (not (Sys.file_exists candidate)));
      let () =
        (* Make a symlink useful to understand what's happening:
           Example: 14219.0>===8173az2===>14220.1 *)
        make_the_symlink
          ~link_suffix:(Printf.sprintf "%s===>%d.%d" channel_tag process thread)
          ~target:basename1
      in
      candidate
    in
    let try_to_create_name_from_exogenous_socketfile1 () =
      let socketfile1 = Option.extract socketfile1 in
      (assert (temp_dir = Filename.dirname socketfile1));
      let basename1 = Filename.basename socketfile1 in
      (* Example: 14219.0<===foo *)
      let candidate = Printf.sprintf "%s/%s%s" temp_dir prefix basename1 in
      (assert (not (Sys.file_exists candidate)));
      let () =
        (* Make a symlink useful to understand what's happening:
           Example: 14219.0>===foo *)
        make_the_symlink ~link_suffix:basename1 ~target:basename1
      in
      candidate
    in
    (try try_to_create_name_from_socketfile1_generated_by_this_library () with _ ->
     try try_to_create_name_from_exogenous_socketfile1 () with _ ->
       create_name_from_socketfile ()
     ) (* end of socketfile0 definition *)
  in
  let sockaddr0 = Unix.ADDR_UNIX socketfile0 in
  let fd0 = make_socket ~bind_to:socketfile0 in
  (fd0, sockaddr0, socketfile0)
;;  

let dgram_input_port_of ?dgram_output_port ~my_stream_inet_addr () =
  let domain = domain_of_inet_addr my_stream_inet_addr in
  let fd0 = Unix.socket domain Unix.SOCK_DGRAM 0 in
  let (sockaddr0, dgram_input_port) =
    let () = 
      match dgram_output_port with
      | None   -> bind fd0 (Unix.ADDR_INET (my_stream_inet_addr, 0))
      | Some p ->
          (* Try to reserve the same port of the client: *)
          try
            Unix.bind fd0 (Unix.ADDR_INET (my_stream_inet_addr, p));
          with e ->
            (* Note here that the exception is Unix.Unix_error(50, "bind", "")
            but for a very strange OCaml (toplevel 3.11.2) behaviour (bug?) the
            pattern Unix.Unix_error (_, _, _) doesn't catch the exception!!! *)
            bind fd0 (Unix.ADDR_INET (my_stream_inet_addr, 0));
    in
    match Unix.getsockname fd0 with
    | (Unix.ADDR_INET (_, assigned_port)) as sockaddr0 -> (sockaddr0, assigned_port)
    | _ -> assert false
  in
  (fd0, sockaddr0, dgram_input_port)
;;

type socketfile = string
type 'a stream_protocol    = stream_channel -> 'a
type 'a seqpacket_protocol = seqpacket_channel -> 'a
type 'a dgram_protocol  = (stream_channel -> dgram_channel) * (dgram_channel -> 'a)

let call_logging_exception ?prefix protocol channel =
  try Either.Right (protocol channel) with e -> ((Log.print_exn ?prefix e); Either.Left e)

let server_fun_of_stream_protocol ?max_input_size (protocol:'a stream_protocol) =
  function fd ->
    let channel = new stream_channel ?max_input_size fd in
    let result =
      call_logging_exception ~prefix:"stream server exception: " protocol channel
    in
    (try channel#shutdown ~receive:() () with _ -> ());
    result

let server_fun_of_seqpacket_protocol ?max_input_size (protocol:'a seqpacket_protocol) =
  function fd ->
    let channel = new seqpacket_channel ?max_input_size fd in
    let result =
      call_logging_exception ~prefix:"seqpacket server exception: " protocol channel
    in
    (try channel#shutdown ~receive:() () with _ -> ());
    result

(* seqpacket - unix *)
let seqpacket_unix_server ?max_pending_requests ?max_input_size ?tutor_behaviour ?no_fork ?socketfile ~(protocol:seqpacket_channel -> unit) () =
  let server_fun = server_fun_of_seqpacket_protocol ?max_input_size protocol in
  unix_server ?max_pending_requests ~seqpacket:() ?tutor_behaviour ?no_fork ?socketfile server_fun

(* stream - unix *)
let stream_unix_server ?max_pending_requests ?max_input_size ?tutor_behaviour ?no_fork ?socketfile ~(protocol:stream_channel -> unit) () =
  let server_fun = server_fun_of_stream_protocol ?max_input_size protocol in
  unix_server ?max_pending_requests ?tutor_behaviour ?no_fork ?socketfile server_fun

(* stream - inet4 *)
let stream_inet4_server ?max_pending_requests ?max_input_size ?tutor_behaviour ?no_fork ?ipv4 ?port ~(protocol:stream_channel -> unit) () =
  let server_fun = server_fun_of_stream_protocol ?max_input_size protocol in
  inet4_server ?max_pending_requests ?tutor_behaviour ?no_fork ?ipv4 ?port server_fun

(* stream - inet6 *)
let stream_inet6_server ?max_pending_requests ?max_input_size ?tutor_behaviour ?no_fork ?ipv6 ?port ~(protocol:stream_channel -> unit) () =
  let server_fun = server_fun_of_stream_protocol ?max_input_size protocol in
  inet6_server ?max_pending_requests ?tutor_behaviour ?no_fork ?ipv6 ?port server_fun


(* stream - inet (both 4 and 6) trying to reserve for ipv6 the same port reserved for ipv4: *)
let stream_inet_server ?max_pending_requests ?max_input_size ?tutor_behaviour ?no_fork ?ipv4 ?ipv6 ?port ~(protocol:stream_channel -> unit) () =
  let server_fun = server_fun_of_stream_protocol ?max_input_size protocol in
  inet_server ?max_pending_requests ?tutor_behaviour ?no_fork ?ipv4 ?ipv6 ?port server_fun

let stream_dgram_protocol_composition
  ~(bootstrap : stream_channel -> dgram_channel)
  ~(protocol  : dgram_channel  -> 'a)
  = fun stream_channel ->
    begin
      let dgram_channel = bootstrap stream_channel in
      (try stream_channel#shutdown ~receive:() () with _ -> ());
      let result = (protocol dgram_channel) in
      (dgram_channel#shutdown ~receive:() ());
      result
    end

(* datagram - unix *)
let dgram_unix_server ?max_pending_requests ?max_input_size ?tutor_behaviour ?no_fork ?socketfile
  ~(bootstrap : stream_channel   -> dgram_channel)
  ~(protocol  : dgram_channel -> unit)
  () =
  let protocol_composition = stream_dgram_protocol_composition ~bootstrap ~protocol in
  let server_fun = server_fun_of_stream_protocol ?max_input_size protocol_composition in
  unix_server ?max_pending_requests ?tutor_behaviour ?no_fork ?socketfile server_fun

(* datagram - inet4 *)
let dgram_inet4_server ?max_pending_requests ?max_input_size ?tutor_behaviour ?no_fork ?ipv4 ?port
  ~(bootstrap : stream_channel   -> dgram_channel)
  ~(protocol  : dgram_channel -> unit)
  () =
  let protocol_composition = stream_dgram_protocol_composition ~bootstrap ~protocol in
  let server_fun = server_fun_of_stream_protocol ?max_input_size protocol_composition in
  inet4_server ?max_pending_requests ?tutor_behaviour ?no_fork ?ipv4 ?port server_fun

(* datagram - inet6 *)
let dgram_inet6_server ?max_pending_requests ?max_input_size ?tutor_behaviour ?no_fork ?ipv6 ?port
  ~(bootstrap : stream_channel   -> dgram_channel)
  ~(protocol  : dgram_channel -> unit)
  () =
  let protocol_composition = stream_dgram_protocol_composition ~bootstrap ~protocol in
  let server_fun = server_fun_of_stream_protocol ?max_input_size protocol_composition in
  inet6_server ?max_pending_requests ?tutor_behaviour ?no_fork ?ipv6 ?port server_fun

(* datagram - inet *)
let dgram_inet_server ?max_pending_requests ?max_input_size ?tutor_behaviour ?no_fork ?ipv4 ?ipv6 ?port
  ~(bootstrap : stream_channel   -> dgram_channel)
  ~(protocol  : dgram_channel -> unit)
  () =
  let protocol_composition = stream_dgram_protocol_composition ~bootstrap ~protocol in
  let server_fun = server_fun_of_stream_protocol ?max_input_size protocol_composition in
  inet_server ?max_pending_requests ?tutor_behaviour ?no_fork ?ipv4 ?ipv6 ?port server_fun


let client ?seqpacket client_fun sockaddr =
  let socket_type =
    match seqpacket with
    | None    -> Unix.SOCK_STREAM
    | Some () -> Unix.SOCK_SEQPACKET (* implies domain = Unix.ADDR_UNIX *)
  in
  let socket = Either.apply_or_catch (Unix.socket (Unix.domain_of_sockaddr sockaddr) socket_type) 0 in
  Either.bind
    socket
    (fun socket ->
      try
	Unix.connect socket sockaddr;
	(try Unix.set_close_on_exec socket with Invalid_argument _ -> ());
	client_fun socket
      with e ->
	begin
	  Unix.close socket;
	  Either.Left (Connecting e)
	end)

let unix_client ?seqpacket ~socketfile client_fun =
  let sockaddr = Unix.ADDR_UNIX socketfile in
  client ?seqpacket client_fun sockaddr

let inet_client ~ipv4_or_v6 ~port client_fun =
  try
    let ipv4_or_v6 = Unix.inet_addr_of_string ipv4_or_v6 in
    let sockaddr = Unix.ADDR_INET (ipv4_or_v6, port) in
    client client_fun sockaddr
  with e -> Either.Left e

(* stream - unix *)
let stream_unix_client ?max_input_size ~socketfile ~(protocol:stream_channel -> 'a) () =
  let client_fun = server_fun_of_stream_protocol ?max_input_size protocol in
  unix_client ~socketfile client_fun

(* seqpacket - unix *)
let seqpacket_unix_client ?max_input_size ~socketfile ~(protocol:'a seqpacket_protocol) () =
  let client_fun = server_fun_of_seqpacket_protocol ?max_input_size protocol in
  unix_client ~seqpacket:() ~socketfile client_fun

(* stream - inet (v4 or v6) *)
let stream_inet_client ?max_input_size ~ipv4_or_v6 ~port ~(protocol:stream_channel -> 'a) () =
  let client_fun = server_fun_of_stream_protocol ?max_input_size protocol in
  inet_client ~ipv4_or_v6 ~port client_fun

(* datagram - unix *)
let dgram_unix_client ?max_input_size ~socketfile
  ~(bootstrap : stream_channel -> dgram_channel)
  ~(protocol  : dgram_channel  -> 'a)
  () =
  let protocol_composition = stream_dgram_protocol_composition ~bootstrap ~protocol in
  let client_fun = server_fun_of_stream_protocol ?max_input_size protocol_composition in
  unix_client ~socketfile client_fun

(* datagram - inet4 or inet6 *)
let dgram_inet_client ?max_input_size
  ~ipv4_or_v6
  ~port
  ~(bootstrap : stream_channel -> dgram_channel)
  ~(protocol  : dgram_channel  -> 'a)
  () =
  let protocol_composition = stream_dgram_protocol_composition ~bootstrap ~protocol in
  let client_fun = server_fun_of_stream_protocol ?max_input_size protocol_composition in
  inet_client ~ipv4_or_v6 ~port client_fun


module Socat = struct

(* The following code is a macro, not a function, in order to bypass the type-system.
   Actually, the type-system doesn't understand the compatibility among a function and
   actuals that are objects of different types. In our case, channel objects may have
   the #receive method slightly different, but all of these methods can be called in a
   default way, just giving them the argument (). The following code should generate
   only this constraint, even if it would not so easy to express. On the contrary,
   the generated constraint is that the function will accept only actuals of type
   < receive : unit -> string; send : string -> unit; .. >.
   We bypass this problem using a macro: *)
DEFINE MACRO_CROSSOVER_LINK (chA,chB) =
  let rec loop_A_to_B () =
    try
      let x  = chA#receive () in
      let () = chB#send x in
      loop_A_to_B ()
    with _ -> chB#shutdown ()
  in
  let rec loop_B_to_A () =
    try
      let x  = chB#receive () in
      let () = chA#send x in
      loop_B_to_A ()
    with _ -> chA#shutdown ()
  in
  let thread_A_to_B = Thread.create loop_A_to_B () in
  let thread_B_to_A = Thread.create loop_B_to_A () in
  Thread.join thread_A_to_B;
  Thread.join thread_B_to_A;
  ()

 (* -------------------------------- *
         of_unix_stream_server
  * -------------------------------- *)

(** Example:
{[# Sys.command "xterm" ;;
  : int = 0

# Sys.command "DISPLAY=127.0.0.1:42 xterm" ;;
xterm Xt error: Can't open display: 127.0.0.1:42
  : int = 1

# Network.Socat.inet4_of_unix_stream_server ~port:6042 ~socketfile:"/tmp/.X11-unix/X0" () ;;
  : Thread.t * string * int = (<abstr>, "0.0.0.0", 6042)

# Sys.command "DISPLAY=127.0.0.1:42 xterm" ;;
  : int = 0 ]} *)
  let inet4_of_unix_stream_server 
    (* inet4 server parameters: *)
    ?max_pending_requests ?max_input_size ?tutor_behaviour ?no_fork ?ipv4 ?port
    (* unix client parameters and inet4 server result: *)
    ~socketfile () : Thread.t * string * int
    =
    stream_inet4_server ?max_pending_requests ?max_input_size ?tutor_behaviour ?no_fork ?ipv4 ?port
      ~protocol:begin fun (chA:stream_channel) ->
	  (* When a connection is accepted the server became a client of the remote unix server: *)
	  ignore (stream_unix_client ?max_input_size ~socketfile
	    ~protocol:begin fun (chB:stream_channel) ->
	        MACRO_CROSSOVER_LINK (chA,chB)
	     end (* client protocol *) ())
       end (* server protocol *) ()

  let inet6_of_unix_stream_server
    (* inet6 server parameters: *)
    ?max_pending_requests ?max_input_size ?tutor_behaviour ?no_fork ?ipv6 ?port
    (* unix client parameters and inet6 server result: *)
    ~socketfile () : Thread.t * string * int
    =
    stream_inet6_server ?max_pending_requests ?max_input_size ?tutor_behaviour ?no_fork ?ipv6 ?port
      ~protocol:begin fun (chA:stream_channel) ->
	  (* When a connection is accepted the server became a client of the remote unix server: *)
	  ignore (stream_unix_client ?max_input_size ~socketfile
	    ~protocol:begin fun (chB:stream_channel) ->
	        MACRO_CROSSOVER_LINK (chA,chB)
	     end (* client protocol *) ())
       end (* server protocol *) ()

  let inet_of_unix_stream_server
    (* inet6 server parameters: *)
    ?max_pending_requests ?max_input_size ?tutor_behaviour ?no_fork ?ipv4 ?ipv6 ?port
    (* unix client parameters and inet6 server result: *)
    ~socketfile () : (Thread.t * string * int) * (Thread.t * string * int)
    =
    stream_inet_server ?max_pending_requests ?max_input_size ?tutor_behaviour ?no_fork ?ipv4 ?ipv6 ?port
      ~protocol:begin fun (chA:stream_channel) ->
	  (* When a connection is accepted the server became a client of the remote unix server: *)
	  ignore (stream_unix_client ?max_input_size ~socketfile
	    ~protocol:begin fun (chB:stream_channel) ->
	        MACRO_CROSSOVER_LINK (chA,chB)
	     end (* client protocol *) ())
       end (* server protocol *) ()


  let unix_of_unix_stream_server
    (* unix server parameters: *)
    ?max_pending_requests ?max_input_size ?tutor_behaviour ?no_fork ?socketfile
    (* unix client parameters and unix server result: *)
    ~dsocketfile () : Thread.t * string
    =
    stream_unix_server ?max_pending_requests ?max_input_size ?tutor_behaviour ?no_fork ?socketfile
      ~protocol:begin fun (chA:stream_channel) ->
	  (* When a connection is accepted the server became a client of the remote unix server: *)
	  ignore (stream_unix_client ?max_input_size ~socketfile:dsocketfile
	    ~protocol:begin fun (chB:stream_channel) ->
	        MACRO_CROSSOVER_LINK (chA,chB)
	     end (* client protocol *) ())
       end (* server protocol *) ()


 (* -------------------------------- *
         of_inet_stream_server
  * -------------------------------- *)

  let unix_of_inet_stream_server
    (* unix server parameters: *)
    ?max_pending_requests ?max_input_size ?tutor_behaviour ?no_fork ?socketfile
    (* inet client parameters and unix server result: *)
    ~ipv4_or_v6 ~port () : Thread.t * string
    =
    stream_unix_server ?max_pending_requests ?max_input_size ?tutor_behaviour ?no_fork ?socketfile
      ~protocol:begin fun (chA:stream_channel) ->
	  (* When a connection is accepted the server became a client of the remote unix server: *)
	  ignore (stream_inet_client ?max_input_size ~ipv4_or_v6 ~port
	    ~protocol:begin fun (chB:stream_channel) ->
	        MACRO_CROSSOVER_LINK (chA,chB)
	     end (* client protocol *) ())
       end (* server protocol *) ()

  let inet4_of_inet_stream_server
    (* inet4 server parameters: *)
    ?max_pending_requests ?max_input_size ?tutor_behaviour ?no_fork ?ipv4 ?port
    (* inet client parameters and inet4 server result: *)
    ~ipv4_or_v6 ~dport () : Thread.t * string * int
    =
    stream_inet4_server ?max_pending_requests ?max_input_size ?tutor_behaviour ?no_fork ?ipv4 ?port
      ~protocol:begin fun (chA:stream_channel) ->
	  (* When a connection is accepted the server became a client of the remote unix server: *)
	  ignore (stream_inet_client ?max_input_size ~ipv4_or_v6 ~port:dport
	    ~protocol:begin fun (chB:stream_channel) ->
	        MACRO_CROSSOVER_LINK (chA,chB)
	     end (* client protocol *) ())
       end (* server protocol *) ()

  let inet6_of_inet_stream_server
    (* inet4 server parameters: *)
    ?max_pending_requests ?max_input_size ?tutor_behaviour ?no_fork ?ipv6 ?port
    (* inet client parameters and inet4 server result: *)
    ~ipv4_or_v6 ~dport () : Thread.t * string * int
    =
    stream_inet6_server ?max_pending_requests ?max_input_size ?tutor_behaviour ?no_fork ?ipv6 ?port
      ~protocol:begin fun (chA:stream_channel) ->
	  (* When a connection is accepted the server became a client of the remote unix server: *)
	  ignore (stream_inet_client ?max_input_size ~ipv4_or_v6 ~port:dport
	    ~protocol:begin fun (chB:stream_channel) ->
	        MACRO_CROSSOVER_LINK (chA,chB)
	     end (* client protocol *) ())
       end (* server protocol *) ()

  let inet_of_inet_stream_server
    (* inet4 server parameters: *)
    ?max_pending_requests ?max_input_size ?tutor_behaviour ?no_fork ?ipv4 ?ipv6 ?port
    (* inet client parameters and inet4 server result: *)
    ~ipv4_or_v6 ~dport () : (Thread.t * string * int) * (Thread.t * string * int)
    =
    stream_inet_server ?max_pending_requests ?max_input_size ?tutor_behaviour ?no_fork ?ipv4 ?ipv6 ?port
      ~protocol:begin fun (chA:stream_channel) ->
	  (* When a connection is accepted the server became a client of the remote unix server: *)
	  ignore (stream_inet_client ?max_input_size ~ipv4_or_v6 ~port:dport
	    ~protocol:begin fun (chB:stream_channel) ->
	        MACRO_CROSSOVER_LINK (chA,chB)
	     end (* client protocol *) ())
       end (* server protocol *) ()

end (* module Socat *)

IFDEF DOCUMENTATION_OR_DEBUGGING THEN
module Examples = struct

(* A simple echo server: *)
let rec simple_echo_server_protocol ch =
  let pr = Printf.kfprintf flush stderr in
  let x  = ch#receive () in
  let () = ch#send x in
  if x="quit"
    then (pr "ECHO server: exiting.\n")
    else simple_echo_server_protocol ch

let rec simple_echo_client_protocol ch =
  let pr = Printf.kfprintf flush stderr in
  pr "Enter the text to send: ";
  let x = try input_line stdin with _ -> "quit" in
  let () = (ch#send x) in
  let y = ch#receive () in
  (if x=y then (pr "Echo received, ok.\n") else (pr "Bad echo!!!!\n"));
  if y="quit"
   then (pr "ECHO client: exiting.\n")
   else simple_echo_client_protocol ch

(* For both inet4 and inet6: *)
let dgram_inet_echo_server ?no_fork ?inet6 ?port () =
  let (thread, ip, port) =
    let bootstrap (ch:stream_channel) =
      (* The client provides the port where it will receive datagrams: *)
      let peer = string_of_sockaddr ch#sockaddr1 in
      Log.printf "Receiving the dgram-inet port number (my output line) from %s\n" peer;
      let dgram_output_port = ch#input_binary_int () in
      let peer_inet_addr = fst (inet_addr_and_port_of_sockaddr ch#sockaddr1) in
      Log.printf "Ok, my output line is %s:%d\n" (Unix.string_of_inet_addr peer_inet_addr) dgram_output_port;
      let sockaddr1 = Unix.ADDR_INET (peer_inet_addr, dgram_output_port) in
      let my_stream_inet_addr = fst (inet_addr_and_port_of_sockaddr ch#sockaddr0) in
      let (fd0, sockaddr0, port0) =
        dgram_input_port_of ~dgram_output_port ~my_stream_inet_addr ()
      in
      let dgram_channel = new dgram_channel ~fd0 ~sockaddr1 () in
      Log.printf "Sending the dgram-inet port number %d (my input line) to %s\n" port0 peer;
      (ch#output_binary_int port0);
      dgram_channel
    in
    let protocol (ch:dgram_channel) =
      simple_echo_server_protocol ch
    in
    match inet6 with
    | None    -> dgram_inet4_server ?no_fork ?port ~bootstrap ~protocol ()
    | Some () -> dgram_inet6_server ?no_fork ?port ~bootstrap ~protocol ()
  in
  (thread, ip, port)

let dgram_unix_echo_server ?no_fork ?stream_socketfile () =
  let stream_socketfile =
    match stream_socketfile with
    | Some x -> x
    | None -> socketname_in_a_fresh_made_directory "ctrl"
  in
  let (t, socketfile) =
    let bootstrap (ch:stream_channel) =
      let sockname = string_of_sockaddr ch#sockaddr0 in
      Log.printf "Receiving the filename (my output line) from %s\n" sockname;
      let dgram_output_socketfile = ch#receive () in
      Log.printf "Ok, my output line is %s\n" dgram_output_socketfile;
      let (fd0, sockaddr0, socketfile0) =
        dgram_input_socketfile_of ~dgram_output_socketfile ~stream_socketfile ()
      in
      let sockaddr1 = Unix.ADDR_UNIX dgram_output_socketfile in
      let dgram_channel = new dgram_channel ~fd0 ~sockaddr1 () in
      Log.printf "Sending the filename %s (my input line) to %s\n" socketfile0 sockname;
      (ch#send socketfile0);
      dgram_channel
    in
    let protocol (ch:dgram_channel) =
      simple_echo_server_protocol ch
    in
    dgram_unix_server ?no_fork ~bootstrap ~protocol ~socketfile:stream_socketfile ()
  in
  (t, socketfile)

let dgram_inet_echo_client ~ipv4_or_v6 ~port () =
  let bootstrap (stream_channel as ch) =
    let my_stream_inet_addr = fst (inet_addr_and_port_of_sockaddr ch#sockaddr0) in
    let (fd0, sockaddr0, port0) =
      dgram_input_port_of ~my_stream_inet_addr ()
    in
    let peer = string_of_sockaddr ch#sockaddr1 in
    Log.printf "Sending the dgram-inet port number %d (my input line) to %s\n" port0 peer;
    (ch#output_binary_int port0);
    Log.printf "Receiving the dgram-inet port number (my output line) from %s\n" peer;
    let dgram_output_port = ch#input_binary_int () in
    let peer_inet_addr =
      fst (inet_addr_and_port_of_sockaddr ch#sockaddr1)
    in
    Log.printf "Ok, my output line is %s:%d\n" (Unix.string_of_inet_addr peer_inet_addr) dgram_output_port;
    let sockaddr1 = Unix.ADDR_INET (peer_inet_addr, dgram_output_port) in
    new dgram_channel ~fd0 ~sockaddr1 ()
  in
  let protocol ch =
    simple_echo_client_protocol ch
  in
  dgram_inet_client ~bootstrap ~protocol ~ipv4_or_v6 ~port ()

let dgram_unix_echo_client ~stream_socketfile () =
  let bootstrap (ch:stream_channel) =
    let (fd0, sockaddr0, socketfile0) =
      dgram_input_socketfile_of ~stream_socketfile ()
    in
    (ch#send socketfile0);
    let socketfile1 = ch#receive () in
    let sockaddr1 = Unix.ADDR_UNIX socketfile1 in
    new dgram_channel ~fd0 ~sockaddr1 ()
  in
  let protocol (ch:dgram_channel) =
    simple_echo_client_protocol ch
  in
  dgram_unix_client ~bootstrap ~protocol ~socketfile:stream_socketfile ()

let stream_unix_echo_server ?no_fork ?socketfile () =
  let socketfile =
    match socketfile with
    | Some x -> x
    | None -> fresh_socketname ()
  in
  let (t, socketfile) =
    let protocol (ch:stream_channel) =
      simple_echo_server_protocol (line_oriented_channel_of_stream_channel ch)
    in
    stream_unix_server ?no_fork ~protocol ~socketfile ()
  in
  (t, socketfile)

let stream_unix_echo_client ~socketfile () =
  let protocol (ch:stream_channel) =
    simple_echo_client_protocol (line_oriented_channel_of_stream_channel ch)
  in
  stream_unix_client ~protocol ~socketfile ()

let seqpacket_unix_echo_server ?no_fork ?socketfile () =
  let socketfile =
    match socketfile with
    | Some x -> x
    | None -> fresh_socketname ()
  in
  let (t, socketfile) =
    let protocol (ch:seqpacket_channel) =
      simple_echo_server_protocol ch
    in
    seqpacket_unix_server ?no_fork ~protocol ~socketfile ()
  in
  (t, socketfile)

let seqpacket_unix_echo_client ~socketfile () =
  let protocol (ch:seqpacket_channel) =
    simple_echo_client_protocol ch
  in
  seqpacket_unix_client ~protocol ~socketfile ()


(* For both inet4 and inet6: *)
let stream_inet_echo_server ?no_fork ?inet6 ?port () =
  let (thread, ip, port) =
    let protocol (ch:stream_channel) =
      simple_echo_server_protocol (line_oriented_channel_of_stream_channel ch)
    in
    match inet6 with
    | None    -> stream_inet4_server ?no_fork ?port ~protocol ()
    | Some () -> stream_inet6_server ?no_fork ?port ~protocol ()
  in
  (thread, ip, port)

let stream_inet_echo_client ~ipv4_or_v6 ~port () =
  let protocol ch =
    simple_echo_client_protocol (line_oriented_channel_of_stream_channel ch)
  in
  stream_inet_client ~protocol ~ipv4_or_v6 ~port ()


end (* module Examples *)
ENDIF