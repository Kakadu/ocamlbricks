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


module Log = Ocamlbricks_log

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

(* From standard library unix.ml *)
let rec accept_non_intr s =
  try Unix.accept s
  with Unix.Unix_error (Unix.EINTR, _, _) -> accept_non_intr s

(* Generic function able to establish a server on a sockaddr. *)
let server ?(max_pending_requests=5) ?seqpacket ?killable ?tutor_behaviour ?only_threads server_fun sockaddr =
  let socket_type =
    match seqpacket with
    | None    -> Unix.SOCK_STREAM
    | Some () -> Unix.SOCK_SEQPACKET (* implies domain = Unix.ADDR_UNIX *)
  in
  let domain = Unix.domain_of_sockaddr sockaddr in
  let listen_socket = Unix.socket domain socket_type 0 in
  (* listen_socket initialization: *)
  let () =
    Unix.setsockopt listen_socket Unix.SO_REUSEADDR true;
    Unix.bind listen_socket sockaddr;
    Unix.listen listen_socket max_pending_requests
  in
  let process_forking_loop () =
    let connexion_no = ref 0 in
    let tutor = ThreadExtra.tutor (*~killable:()*) ?behaviour:tutor_behaviour () in
    while true do
      let (service_socket, _) = accept_non_intr listen_socket in
      incr connexion_no;
      let sockaddr0 = string_of_sockaddr (Unix.getsockname service_socket) in
      let sockaddr1 = string_of_sockaddr (Unix.getpeername service_socket) in
      Log.printf "Accepted connection #%d on %s from %s\n" !connexion_no sockaddr0 sockaddr1;
      match Unix.fork () with
      |	0 ->
          (* The child here: *)
          begin
            Log.printf "Process Fork: activated for connection #%d on %s\n" !connexion_no sockaddr0;
	    Unix.close listen_socket;
	    (try Unix.set_close_on_exec service_socket with Invalid_argument _ -> ());
	    let () = server_fun service_socket in
	    exit 0
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
    while true do
      let (service_socket, _) = accept_non_intr listen_socket in
      ignore (Thread.create server_fun service_socket);
     (* Unix.close service_socket; (* really need or bad? *)*)
    done
  in
  let forking_loop () =
    (* listen_socket finalization: *)
    let () =
      match sockaddr with
      | Unix.ADDR_UNIX filename ->
	  ThreadExtra.at_exit (fun () -> Unix.unlink filename)
      | _ -> ()
    in
    (* process or thread switching: *)
    match only_threads with
    | None    -> process_forking_loop ()
    | Some () -> thread_forking_loop ()
  in
  let server_thread = ThreadExtra.create ?killable forking_loop () in
  server_thread
  

let unix_server ?max_pending_requests ?seqpacket ?killable ?tutor_behaviour ?only_threads ?filename server_fun =
  let filename = match filename with
    | Some x -> x
    | None ->
        let prefix = "."^(Filename.basename (Sys.executable_name))^"-" in
        let suffix = "-unix" in
        let temp_dir =
          let result = Filename.temp_file prefix suffix in
          Sys.remove result;
          Unix.mkdir result 0o600;
          result
        in
        (temp_dir^"/listen-socket")
  in
  let sockaddr = Unix.ADDR_UNIX filename in
  let server_thread = server ?max_pending_requests ?seqpacket ?killable ?tutor_behaviour ?only_threads server_fun sockaddr in
  (server_thread, filename)


let inet_server ?max_pending_requests ?seqpacket ?killable ?tutor_behaviour ?only_threads ?ipv4 ~port server_fun =
  let ipv4 = match ipv4 with
    | Some x -> Unix.inet_addr_of_string x
    | None   -> Unix.inet_addr_any
  in
  let sockaddr = Unix.ADDR_INET (ipv4, port) in
  let server_thread = server ?max_pending_requests ?seqpacket ?killable ?tutor_behaviour ?only_threads server_fun sockaddr in
  (server_thread, (Unix.string_of_inet_addr ipv4))

let inet6_server ?max_pending_requests ?seqpacket ?killable ?tutor_behaviour ?only_threads ?ipv6 ~port server_fun =
  let ipv6 = match ipv6 with
    | Some x -> Unix.inet_addr_of_string x
    | None   -> Unix.inet6_addr_any
  in
  let sockaddr = Unix.ADDR_INET (ipv6, port) in
  let server_thread = server ?max_pending_requests ?seqpacket ?killable ?tutor_behaviour ?only_threads server_fun sockaddr in
  (server_thread, (Unix.string_of_inet_addr ipv6))


(* High-level representation of the structure available, after a connection, to both endpoints.
   The max_input_size is set by default to 1514 (Ethernet: 1514=1526-12 (8 preamble and 4 CRC)) *)
class stream_or_seqpacket_bidirectional_channel ?(max_input_size=1514) ?seqpacket fd =
  let input_buffer = String.create max_input_size in
  (* Send method for stream socket descriptors: *)
  let rec send_stream_loop x off len =
    if len=0 then () else
    let n = Unix.send fd x off len [] in
    if n = 0 then failwith "channel#send: failed to send in a stream channel: no more than 0 bytes sent!" else
    if n<len then send_stream_loop x (off+n) (len-n) else
    ()
  in
  (* Send method for seqpacket socket descriptors: *)
  let send_seqpacket x off len =
    let n = Unix.send fd x off len [] in
    if n<len then failwith (Printf.sprintf "channel#send: failed sending a seqpacket: no more than %d bytes sent!" n) else
    ()
  in
  (* Select the good method at object creation time: *)
  let send_implementation =
    match seqpacket with
    | None    -> send_stream_loop
    | Some () -> send_seqpacket
  in
  let () =
    let min_byte_no_for_input  = Unix.getsockopt_int fd Unix.SO_RCVLOWAT in
    let max_byte_no_for_output = Unix.getsockopt_int fd Unix.SO_SNDLOWAT in
    Log.printf "Currently  min_byte_no_for_input=%d  and  max_byte_no_for_output=%d\n"
    min_byte_no_for_input max_byte_no_for_output;
    let timeout_for_input  = Unix.getsockopt_float fd Unix.SO_RCVTIMEO in
    let timeout_for_output = Unix.getsockopt_float fd Unix.SO_SNDTIMEO in
    Log.printf "Currently  timeout_for_input=%F  and  timeout_for_output=%F\n"
    timeout_for_input timeout_for_output;
(*    Unix.setsockopt_int fd Unix.SO_RCVLOWAT 1;
    Unix.setsockopt_int fd Unix.SO_SNDLOWAT 1;*)
  in
  object

  method receive : string =
    try
      let n = Unix.recv fd input_buffer 0 max_input_size [] in
      (if n=0 then failwith "channel#receive: received 0 bytes (peer terminated?)");
      String.sub input_buffer 0 n
    with e ->
      Log.print_exn ~prefix:"channel#receive: " e;
      raise e

  method peek : string option =
    try
      let n = Unix.recv fd input_buffer 0 max_input_size [Unix.MSG_PEEK] in
      if n>0 then Some (String.sub input_buffer 0 n) else None
    with e ->
      Log.print_exn ~prefix:"channel#peek: " e;
      None

  method send (x:string) : unit =
    send_implementation x 0 (String.length x)

  method shutdown ?receive ?send () =
    try
      let shutdown_command =
	match receive, send with
	| None, None | Some (), Some () -> Unix.SHUTDOWN_ALL
	| None, Some () -> Unix.SHUTDOWN_SEND
	| Some (), None -> Unix.SHUTDOWN_RECEIVE
      in
      Unix.shutdown fd shutdown_command
    with e ->
      Log.print_exn ~prefix:"channel#shutdown: " e;
      raise e

  method sockaddr0 = Unix.getsockname fd
  method sockaddr1 = Unix.getpeername fd

end (* class stream_or_seqpacket_bidirectional_channel *)

class stream_channel ?max_input_size fd =
  let in_channel  = Unix.in_channel_of_descr  fd in
  let out_channel = Unix.out_channel_of_descr fd in
  let raise_but_also_log_it caller e =
    let prefix = Printf.sprintf "stream_channel#%s: " caller in
    let () = Log.print_exn ~prefix e in
    raise e
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
    with e -> raise_but_also_log_it caller e
  in
  object
    inherit stream_or_seqpacket_bidirectional_channel ?max_input_size fd

    method input_char       : char   = tutor0 Pervasives.input_char in_channel "input_char"
    method input_line       : string = tutor0 Pervasives.input_line in_channel "input_line"
    method input_byte       : int    = tutor0 Pervasives.input_byte in_channel "input_byte"
    method input_binary_int : int    = tutor0 Pervasives.input_binary_int in_channel "input_binary_int"
    method input_value      : 'a. 'a = tutor0 Pervasives.input_value in_channel "input_value"

    method output_char   x = tutor1 Pervasives.output_char out_channel x "output_char"
    method output_string x = tutor1 Pervasives.output_string out_channel x "output_string"
    method output_byte   x = tutor1 Pervasives.output_byte out_channel x "output_byte"
    method output_binary_int x = tutor1 Pervasives.output_binary_int out_channel x "output_binary_int"
    method output_value : 'a. 'a -> unit =
      fun x -> tutor1 Pervasives.output_value out_channel x "output_value"

end (* class stream_channel *)


class seqpacket_channel ?max_input_size fd =
  object
    inherit stream_or_seqpacket_bidirectional_channel ?max_input_size ~seqpacket:() fd
end (* class seqpacket_channel *)

exception Unexpected_sender of string

(* Typically the client builds its socketfile (0), send it to the server through the stream channel, then
   receives its socketfile for output (1).  *)
class dgram_channel ?(max_input_size=1514) ~fd0 ~sockaddr1 () =
  let input_buffer = String.create max_input_size in
  let sockaddr0 = Unix.getsockname fd0 in
  object

  method receive : string =
    try
      let (n, sockaddr) = Unix.recvfrom fd0 input_buffer 0 max_input_size [] in
      (if sockaddr <> sockaddr1 then raise (Unexpected_sender (string_of_sockaddr sockaddr)));
      String.sub input_buffer 0 n
    with e ->
      Log.print_exn ~prefix:"dgram_channel#receive: " e;
      raise e

  method peek : string option =
    try  
      let (n, sockaddr) = Unix.recvfrom fd0 input_buffer 0 max_input_size [Unix.MSG_PEEK] in
      (if sockaddr <> sockaddr1 then raise (Unexpected_sender (string_of_sockaddr sockaddr)));
      if n>0 then Some (String.sub input_buffer 0 n) else None
    with e ->
      Log.print_exn ~prefix:"dgram_channel#peek: " e;
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
      raise e
    
  method shutdown ?receive ?send () =
    try
      let shutdown_command =
	match receive, send with
	| None, None | Some (), Some () -> Unix.SHUTDOWN_ALL
	| None, Some () -> Unix.SHUTDOWN_SEND
	| Some (), None -> Unix.SHUTDOWN_RECEIVE
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
      raise e


end (* class dgram_channel *)


let dgram_input_socketfile_of ?dgram_output_socketfile ~stream_socketfile () =
  let make_socket ~bind_to =
    let result = Unix.socket Unix.PF_UNIX Unix.SOCK_DGRAM 0 in
    let socketfile = bind_to in
    Unix.bind result (Unix.ADDR_UNIX socketfile);
    Log.printf "Unix datagram socket bound to %s\n" socketfile;
    result
  in
  let socketfile1 = dgram_output_socketfile in
  let socketfile0 =
    let temp_dir = Filename.dirname stream_socketfile in
    let prefix = "wire--" in
    let suffix = Printf.sprintf "--to--%d.%d" (Unix.getpid ()) (Thread.id (Thread.self ())) in
    let create_name_from_socketfile () =
      (* Filename.temp_file add an hexadecimal string between the prefix and the suffix: *)
      let result = Filename.temp_file ~temp_dir prefix suffix in
      let () = Unix.unlink result in
      result
    in
    let try_to_create_name_from_socketfile1 () =
      let socketfile1 = Option.extract socketfile1 in
      (assert (temp_dir = Filename.dirname socketfile1));
      let basename1 = Filename.basename socketfile1 in
      let inner_hex_string = 
        Scanf.sscanf basename1 "wire--%x--to--%d.%d" (fun s p t -> Printf.sprintf "%x" s)
      in
      let candidate = Printf.sprintf "%s/%s%s%s" temp_dir prefix inner_hex_string suffix in
      (assert (not (Sys.file_exists candidate)));
      candidate
    in
    (try
      try_to_create_name_from_socketfile1 ()
     with
      _ -> create_name_from_socketfile ()
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
      | None   -> Unix.bind fd0 (Unix.ADDR_INET (my_stream_inet_addr, 0))
      | Some p ->
          (* Try to reserve the same port of the client: *)
          try
            Unix.bind fd0 (Unix.ADDR_INET (my_stream_inet_addr, p));
          with e ->
            (* Note here that the exception is Unix.Unix_error(50, "bind", "")
            but for a very strange OCaml (toplevel 3.11.2) behaviour (bug?) the
            pattern Unix.Unix_error (_, _, _) doesn't catch the exception!!! *)
            Unix.bind fd0 (Unix.ADDR_INET (my_stream_inet_addr, 0));
    in
    match Unix.getsockname fd0 with
    | (Unix.ADDR_INET (_, assigned_port)) as sockaddr0 -> (sockaddr0, assigned_port)
    | _ -> assert false
  in
  (fd0, sockaddr0, dgram_input_port)
;;

type socketfile = string
type stream_protocol    = stream_channel -> unit
type seqpacket_protocol = seqpacket_channel -> unit
type dgram_protocol  = (stream_channel -> dgram_channel) * (dgram_channel -> unit)

let server_fun_of_stream_protocol ?max_input_size protocol =
  function fd ->
    let channel = new stream_channel ?max_input_size fd in
    let result = protocol channel in
    (try channel#shutdown ~receive:() () with _ -> ());
    result

let server_fun_of_seqpacket_protocol ?max_input_size protocol =
  function fd ->
    let channel = new seqpacket_channel ?max_input_size fd in
    let result = protocol channel in
    (try channel#shutdown ~receive:() () with _ -> ());
    result

(* seqpacket - unix *)
let seqpacket_unix_server ?max_pending_requests ?max_input_size ?killable ?tutor_behaviour ?only_threads ?filename ~(protocol:seqpacket_channel -> unit) () =
  let server_fun = server_fun_of_seqpacket_protocol ?max_input_size protocol in
  unix_server ?max_pending_requests ~seqpacket:() ?killable ?tutor_behaviour ?only_threads ?filename server_fun

(* stream - unix *)
let stream_unix_server ?max_pending_requests ?max_input_size ?killable ?tutor_behaviour ?only_threads ?filename ~(protocol:stream_channel -> unit) () =
  let server_fun = server_fun_of_stream_protocol ?max_input_size protocol in
  unix_server ?max_pending_requests ?killable ?tutor_behaviour ?only_threads ?filename server_fun

(* stream - inet *)
let stream_inet_server ?max_pending_requests ?max_input_size ?killable ?tutor_behaviour ?only_threads ?ipv4 ~port ~(protocol:stream_channel -> unit) () =
  let server_fun = server_fun_of_stream_protocol ?max_input_size protocol in
  inet_server ?max_pending_requests ?killable ?tutor_behaviour ?only_threads ?ipv4 ~port server_fun

(* stream - inet6 *)
let stream_inet6_server ?max_pending_requests ?max_input_size ?killable ?tutor_behaviour ?only_threads ?ipv6 ~port ~(protocol:stream_channel -> unit) () =
  let server_fun = server_fun_of_stream_protocol ?max_input_size protocol in
  inet6_server ?max_pending_requests ?killable ?tutor_behaviour ?only_threads ?ipv6 ~port server_fun


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
let dgram_unix_server ?max_pending_requests ?max_input_size ?killable ?tutor_behaviour ?only_threads ?filename
  ~(bootstrap : stream_channel   -> dgram_channel)
  ~(protocol  : dgram_channel -> unit)
  () =
  let protocol_composition = stream_dgram_protocol_composition ~bootstrap ~protocol in
  let server_fun = server_fun_of_stream_protocol ?max_input_size protocol_composition in
  unix_server ?max_pending_requests ?killable ?tutor_behaviour ?only_threads ?filename server_fun

(* datagram - inet *)
let dgram_inet_server ?max_pending_requests ?max_input_size ?killable ?tutor_behaviour ?only_threads ?ipv4 ~port 
  ~(bootstrap : stream_channel   -> dgram_channel)
  ~(protocol  : dgram_channel -> unit)
  () =
  let protocol_composition = stream_dgram_protocol_composition ~bootstrap ~protocol in
  let server_fun = server_fun_of_stream_protocol ?max_input_size protocol_composition in
  inet_server ?max_pending_requests ?killable ?tutor_behaviour ?only_threads ?ipv4 ~port server_fun

(* datagram - inet6 *)
let dgram_inet6_server ?max_pending_requests ?max_input_size ?killable ?tutor_behaviour ?only_threads ?ipv6 ~port
  ~(bootstrap : stream_channel   -> dgram_channel)
  ~(protocol  : dgram_channel -> unit)
  () =
  let protocol_composition = stream_dgram_protocol_composition ~bootstrap ~protocol in
  let server_fun = server_fun_of_stream_protocol ?max_input_size protocol_composition in
  inet6_server ?max_pending_requests ?killable ?tutor_behaviour ?only_threads ?ipv6 ~port server_fun


(* For both inet4 and inet6: *)
let dgram_inet_echo_server ?inet6 ~port () =
  let (thread, _) =
    let bootstrap (stream_channel as ch) =
      (* The client provides the port where it will receive datagrams: *)
      let peer = string_of_sockaddr ch#sockaddr1 in
      Log.printf "Receiving the dgram-inet port number (my output line) from %s\n" peer;
      let dgram_output_port = ch#input_binary_int in
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
    (* A simple echo server: *)
    let rec protocol ch =
      let x = ch#receive in
      (ch#send x);
      if x="quit"
       then (Printf.kfprintf flush stderr "Datagram inet ECHO server exiting.\n")
       else protocol ch
    in
    match inet6 with
    | None    -> dgram_inet_server  ~port ~bootstrap ~protocol ()
    | Some () -> dgram_inet6_server ~port ~bootstrap ~protocol ()
  in
  thread


(* Esempio: *)
let dgram_unix_echo_server ~stream_socketfile () =
  let (t, socketfile) =
    let bootstrap s =
      let dgram_output_socketfile = s#receive in
      let (fd0, sockaddr0, socketfile0) =
        dgram_input_socketfile_of ~dgram_output_socketfile ~stream_socketfile ()
      in
      let sockaddr1 = Unix.ADDR_UNIX dgram_output_socketfile in
      let ch = new dgram_channel ~fd0 ~sockaddr1 () in
      (s#send socketfile0);
      ch
    in
    (* A simple echo server: *)
    let rec protocol ch =
      let x = ch#receive in
      (ch#send x);
      if x="quit"
       then (Printf.kfprintf flush stderr "Datagram unix ECHO server exiting.\n")
       else protocol ch
    in
    dgram_unix_server ~bootstrap ~protocol ~filename:stream_socketfile ()
  in
  (t, socketfile)


let client ?seqpacket client_fun sockaddr =
  let socket_type =
    match seqpacket with
    | None    -> Unix.SOCK_STREAM
    | Some () -> Unix.SOCK_SEQPACKET (* implies domain = Unix.ADDR_UNIX *)
  in
  let socket = Unix.socket (Unix.domain_of_sockaddr sockaddr) socket_type 0 in
  try
    Unix.connect socket sockaddr;
    (try Unix.set_close_on_exec socket with Invalid_argument _ -> ());
    client_fun socket
  with e ->
    begin
      Unix.close socket;
      raise e
    end

let unix_client ?seqpacket ~filename client_fun =
  let sockaddr = Unix.ADDR_UNIX filename in
  client ?seqpacket client_fun sockaddr

let inet_client ~ipv4_or_v6 ~port client_fun =
  let ipv4_or_v6 = Unix.inet_addr_of_string ipv4_or_v6 in
  let sockaddr = Unix.ADDR_INET (ipv4_or_v6, port) in
  client client_fun sockaddr

(* seqpacket - unix *)
let seqpacket_unix_client ?max_input_size ~filename ~(protocol:seqpacket_channel -> 'a) () =
  let client_fun = server_fun_of_seqpacket_protocol ?max_input_size protocol in
  unix_client ~seqpacket:() ~filename client_fun

(* stream - unix *)
let stream_unix_client ?max_input_size ~filename ~(protocol:stream_channel -> 'a) () =
  let client_fun = server_fun_of_stream_protocol ?max_input_size protocol in
  unix_client ~filename client_fun

(* stream - inet (v4 or v6) *)
let stream_inet_client ?max_input_size ~ipv4_or_v6 ~port ~(protocol:stream_channel -> 'a) () =
  let client_fun = server_fun_of_stream_protocol ?max_input_size protocol in
  inet_client ~ipv4_or_v6 ~port client_fun

(* datagram - unix *)
let dgram_unix_client ?max_input_size ~filename
  ~(bootstrap : stream_channel -> dgram_channel)
  ~(protocol  : dgram_channel  -> 'a)
  () =
  let protocol_composition = stream_dgram_protocol_composition ~bootstrap ~protocol in
  let client_fun = server_fun_of_stream_protocol ?max_input_size protocol_composition in
  unix_client ~filename client_fun

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

let dgram_inet_echo_client ~ipv4_or_v6 ~port () =
  let pr = Printf.kfprintf flush stderr in
  let bootstrap (stream_channel as ch) =
    let my_stream_inet_addr = fst (inet_addr_and_port_of_sockaddr ch#sockaddr0) in
    let (fd0, sockaddr0, port0) =
      dgram_input_port_of ~my_stream_inet_addr ()
    in
    let peer = string_of_sockaddr ch#sockaddr1 in
    Log.printf "Sending the dgram-inet port number %d (my input line) to %s\n" port0 peer;
    (ch#output_binary_int port0);
    Log.printf "Receiving the dgram-inet port number (my output line) from %s\n" peer;
    let dgram_output_port = ch#input_binary_int in
    let peer_inet_addr =
      fst (inet_addr_and_port_of_sockaddr ch#sockaddr1)
    in
    Log.printf "Ok, my output line is %s:%d\n" (Unix.string_of_inet_addr peer_inet_addr) dgram_output_port;
    let sockaddr1 = Unix.ADDR_INET (peer_inet_addr, dgram_output_port) in
    new dgram_channel ~fd0 ~sockaddr1 ()
  in
  let rec protocol ch =
    pr "Enter the text to send: ";
    let x = input_line stdin in
    (ch#send x);
    let y = ch#receive in
    (if x=y then (pr "Echo received, ok.\n") else (pr "Bad echo!!!!\n"));
    if y="quit" then (pr "client: QUIT!!!!\n") else protocol ch
  in
  dgram_inet_client ~bootstrap ~protocol ~ipv4_or_v6 ~port ()

let dgram_unix_echo_client ~stream_socketfile () =
  let pr = Printf.kfprintf flush stderr in
  let bootstrap s =
    let (fd0, sockaddr0, socketfile0) =
      dgram_input_socketfile_of ~stream_socketfile ()
    in
    (s#send socketfile0);
    let socketfile1 = s#receive in
    let sockaddr1 = Unix.ADDR_UNIX socketfile1 in
    new dgram_channel ~fd0 ~sockaddr1 ()
  in
  let rec protocol ch =
    pr "Enter the text to send: ";
    let x = input_line stdin in
    (ch#send x);
    let y = ch#receive in
    (if x=y then (pr "Echo received, ok.\n") else (pr "Bad echo!!!!\n"));
    if y="quit" then (pr "client: QUIT!!!!\n") else protocol ch
  in
  dgram_unix_client ~bootstrap ~protocol ~filename:stream_socketfile ()
