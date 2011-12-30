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

(* Extract the name of the associated socket file from a unix socket.
    Raises [Not_found] if the socket is not in the unix domain. *)
let extract_socketfile = function
  | Unix.ADDR_UNIX x -> x
  | _ -> raise Not_found

let string_of_sockaddr = function
  | Unix.ADDR_UNIX x -> x
  | Unix.ADDR_INET (inet_addr, port) ->
      Printf.sprintf "%s:%d" (Unix.string_of_inet_addr inet_addr) port

(* From standard library unix.ml *)
let rec accept_non_intr s =
  try Unix.accept s
  with Unix.Unix_error (Unix.EINTR, _, _) -> accept_non_intr s

(* From standard library unix.ml *)
let rec waitpid_non_intr pid =
  try Unix.waitpid [] pid
  with Unix.Unix_error (Unix.EINTR, _, _) -> waitpid_non_intr pid

(* Generic function able to establish a server on a sockaddr. *)
let server ?(max_pending_requests=5) ?seqpacket ?process_tutor ?only_threads server_fun sockaddr =
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
    let process_tutor =
      match process_tutor with
      | None   ->
         (function pid ->
            Log.printf "Default tutor started for the process %d\n" pid;
            ignore (waitpid_non_intr pid)
            )
      | Some f ->
         (function pid ->
            Log.printf "Provided tutor started for the process %d\n" pid;
            let () = f ~pid in ignore (waitpid_non_intr pid)
            )
    in
    while true do
      let (service_socket, _) = accept_non_intr listen_socket in
      incr connexion_no;
      let sockaddr = string_of_sockaddr (Unix.getsockname service_socket) in
      Log.printf "Accepted connection #%d from %s\n" !connexion_no sockaddr;
      match Unix.fork () with
      |	0 ->
          (* The child here: *)
          begin
            Log.printf "Process started to serve the connection #%d from %s\n" !connexion_no sockaddr;
	    Unix.close listen_socket;
	    (try Unix.set_close_on_exec service_socket with Invalid_argument _ -> ());
	    let () = server_fun service_socket in
	    exit 0
	  end
      | child_id ->
          (* The father here creates a process-tutor thread per child: *)
          begin
            Unix.close service_socket;
            ignore (ThreadExtra.create process_tutor child_id)
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
  let server_thread = ThreadExtra.create forking_loop () in
  server_thread
  

let unix_domain_server ?max_pending_requests ?seqpacket ?process_tutor ?only_threads ?filename server_fun =
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
  let server_thread = server ?max_pending_requests ?seqpacket ?process_tutor ?only_threads server_fun sockaddr in
  (server_thread, filename)


let inet_domain_server ?max_pending_requests ?seqpacket ?process_tutor ?only_threads ?ipv4 ~port server_fun =
  let ipv4 = match ipv4 with
    | Some x -> Unix.inet_addr_of_string x
    | None   -> Unix.inet_addr_any
  in
  let sockaddr = Unix.ADDR_INET (ipv4, port) in
  let server_thread = server ?max_pending_requests ?seqpacket ?process_tutor ?only_threads server_fun sockaddr in
  (server_thread, (Unix.string_of_inet_addr ipv4))

let inet6_domain_server ?max_pending_requests ?seqpacket ?process_tutor ?only_threads ?ipv6 ~port server_fun =
  let ipv6 = match ipv6 with
    | Some x -> Unix.inet_addr_of_string x
    | None   -> Unix.inet6_addr_any
  in
  let sockaddr = Unix.ADDR_INET (ipv6, port) in
  let server_thread = server ?max_pending_requests ?seqpacket ?process_tutor ?only_threads server_fun sockaddr in
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
    if n <len then failwith (Printf.sprintf "channel#send: failed sending a seqpacket: no more than %d bytes sent!" n) else
    ()
  in
  (* Select the good method at object creation time: *)
  let send_implementation =
    match seqpacket with
    | None    -> send_stream_loop
    | Some () -> send_seqpacket
  in
  object

  method receive : string = 
    let n = Unix.recv fd input_buffer 0 max_input_size [] in
    String.sub input_buffer 0 n

  method peek : string =
    let n = Unix.recv fd input_buffer 0 max_input_size [Unix.MSG_PEEK] in
    String.sub input_buffer 0 n

  method send (x:string) : unit =
    send_implementation x 0 (String.length x)

  method shutdown ?receive ?send () =
    let shutdown_command =
      match receive, send with
      | None, None | Some (), Some () -> Unix.SHUTDOWN_ALL
      | None, Some () -> Unix.SHUTDOWN_SEND
      | Some (), None -> Unix.SHUTDOWN_RECEIVE
    in
    Unix.shutdown fd shutdown_command

end (* class stream_or_seqpacket_bidirectional_channel *)

class stream_channel ?max_input_size fd =
  let in_channel  = Unix.in_channel_of_descr  fd in
  let out_channel = Unix.out_channel_of_descr fd in
  object
    inherit stream_or_seqpacket_bidirectional_channel ?max_input_size fd

    method input_char       : char   = Pervasives.input_char in_channel
    method input_line       : string = Pervasives.input_line in_channel
    method input_byte       : int    = Pervasives.input_byte in_channel
    method input_binary_int : int    = Pervasives.input_binary_int in_channel
    method input_value      : 'a. 'a = Pervasives.input_value in_channel

    method output_char       : char -> unit   = Pervasives.output_char out_channel
    method output_string     : string -> unit = Pervasives.output_string out_channel
    method output_byte       : int -> unit    = Pervasives.output_byte out_channel
    method output_binary_int : int -> unit    = Pervasives.output_binary_int out_channel
    method output_value      : 'a. 'a -> unit = Pervasives.output_value out_channel

end (* class stream_channel *)


class seqpacket_channel ?max_input_size fd =
  object
    inherit stream_or_seqpacket_bidirectional_channel ?max_input_size ~seqpacket:() fd
end (* class seqpacket_channel *)

(* socketfile0 may be not provided when we are building a server.
   Typically the client builds its socketfile (0), send it to the server through the stream channel, then
   receives its socketfile for output (1).  *)
class datagram_channel ?(max_input_size=1514) ~fd0 ~sockaddr1 () =
  let input_buffer = String.create max_input_size in
  let sockaddr0 = Unix.getsockname fd0 in
  object

  method receive : string =
    let (n, sockaddr) = Unix.recvfrom fd0 input_buffer 0 max_input_size [] in
    (assert (sockaddr = sockaddr1));
    String.sub input_buffer 0 n

  method peek : string =
    let (n, sockaddr) = Unix.recvfrom fd0 input_buffer 0 max_input_size [Unix.MSG_PEEK] in
    (assert (sockaddr = sockaddr1));
    String.sub input_buffer 0 n

  method send (x:string) : unit =
    let len = String.length x in
    (* fd0 represents where I want to receive the answer: *)
    let n = Unix.sendto fd0 x 0 len [] sockaddr1 in
    if n<len then failwith (Printf.sprintf "datagram_channel#send: no more than %d bytes sent (instead of %d)" n len) else
    ()
    
  method shutdown ?receive ?send () =
    let shutdown_command =
      match receive, send with
      | None, None | Some (), Some () -> Unix.SHUTDOWN_ALL
      | None, Some () -> Unix.SHUTDOWN_SEND
      | Some (), None -> Unix.SHUTDOWN_RECEIVE
    in
    (match shutdown_command with
    | Unix.SHUTDOWN_RECEIVE | Unix.SHUTDOWN_ALL ->
        (try Unix.close fd0 with _ -> ());
        (try Unix.unlink (extract_socketfile sockaddr0) with _ -> ());
    | _ -> ()
    );
    (match shutdown_command with
    | Unix.SHUTDOWN_SEND | Unix.SHUTDOWN_ALL ->
        (try Unix.unlink (extract_socketfile sockaddr1) with _ -> ());
    | _ -> ()
    );

end (* class datagram_channel *)

(* Constructor: *)
let make_derived_unix_datagram_socket ?socketfile1 ~socketfile () =
  let make_socket ~bind_to =
    let result = Unix.socket Unix.PF_UNIX Unix.SOCK_DGRAM 0 in
    let socketfile = bind_to in
    Log.printf "Socket will bound to %s\n" socketfile;
    Unix.bind result (Unix.ADDR_UNIX socketfile);
    result
  in
  let socketfile0 =
    let temp_dir = Filename.dirname socketfile in
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

type socketfile = string
type stream_protocol    = stream_channel -> unit
type seqpacket_protocol = seqpacket_channel -> unit
type datagram_protocol  = (stream_channel -> datagram_channel) * (datagram_channel -> unit)

let server_fun_of_stream_protocol ?max_input_size protocol =
  function fd ->
    let channel = new stream_channel ?max_input_size fd in
    let result = protocol channel in
    (try channel#shutdown () with _ -> ());
    result

let server_fun_of_seqpacket_protocol ?max_input_size protocol =
  function fd ->
    let channel = new seqpacket_channel ?max_input_size fd in
    let result = protocol channel in
    (try channel#shutdown () with _ -> ());
    result


(* seqpacket - unix *)
let seqpacket_unix_domain_server ?max_pending_requests ?max_input_size ?process_tutor ?only_threads ?filename ~(protocol:seqpacket_channel -> unit) () =
  let server_fun = server_fun_of_seqpacket_protocol ?max_input_size protocol in
  unix_domain_server ?max_pending_requests ~seqpacket:() ?process_tutor ?only_threads ?filename server_fun

(* stream - unix *)
let stream_unix_domain_server ?max_pending_requests ?max_input_size ?process_tutor ?only_threads ?filename ~(protocol:stream_channel -> unit) () =
  let server_fun = server_fun_of_stream_protocol ?max_input_size protocol in
  unix_domain_server ?max_pending_requests ?process_tutor ?only_threads ?filename server_fun

(* stream - inet *)
let stream_inet_domain_server ?max_pending_requests ?max_input_size ?process_tutor ?only_threads ?ipv4 ~port ~(protocol:stream_channel -> unit) () =
  let server_fun = server_fun_of_stream_protocol ?max_input_size protocol in
  inet_domain_server ?max_pending_requests ?process_tutor ?only_threads ?ipv4 ~port server_fun

(* stream - inet6 *)
let stream_inet6_domain_server ?max_pending_requests ?max_input_size ?process_tutor ?only_threads ?ipv6 ~port ~(protocol:stream_channel -> unit) () =
  let server_fun = server_fun_of_stream_protocol ?max_input_size protocol in
  inet6_domain_server ?max_pending_requests ?process_tutor ?only_threads ?ipv6 ~port server_fun

(* datagram - unix *)
let datagram_unix_domain_server ?max_pending_requests ?max_input_size ?process_tutor ?only_threads ?filename
  ~(bootstrap : stream_channel   -> datagram_channel)
  ~(protocol  : datagram_channel -> unit)
  () =
  let protocol_composition =
    fun stream_channel ->
      let datagram_channel = bootstrap stream_channel in
      (try stream_channel#shutdown () with _ -> ());
      (protocol datagram_channel);
      datagram_channel#shutdown ~receive:() ()
  in
  let server_fun = server_fun_of_stream_protocol ?max_input_size protocol_composition in
  unix_domain_server ?max_pending_requests ?process_tutor ?only_threads ?filename server_fun

(* Esempio: *)
let echo_server ~filename () =
  let (t, socketfile) =
    let bootstrap s =
      let socketfile1 = s#receive in
      let (fd0, sockaddr0, socketfile0) = make_derived_unix_datagram_socket ~socketfile1 ~socketfile:filename () in
      let sockaddr1 = Unix.ADDR_UNIX socketfile1 in
      let ch = new datagram_channel ~fd0 ~sockaddr1 () in
      (s#send socketfile0);
      ch
    in
    (* A simple echo server: *)
    let rec protocol ch =
      let x = ch#receive in
      (ch#send x);
      if x="quit" then (Printf.kfprintf flush stderr "Echo server exiting.\n") else protocol ch
    in
    datagram_unix_domain_server ~bootstrap ~protocol ~filename ()
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

let unix_domain_client ?seqpacket ~filename client_fun =
  let sockaddr = Unix.ADDR_UNIX filename in
  client ?seqpacket client_fun sockaddr

let inet_domain_client ~ipv4_or_v6 ~port client_fun =
  let ipv4_or_v6 = Unix.inet_addr_of_string ipv4_or_v6 in
  let sockaddr = Unix.ADDR_INET (ipv4_or_v6, port) in
  client client_fun sockaddr

(* seqpacket - unix *)
let seqpacket_unix_domain_client ?max_input_size ~filename ~(protocol:seqpacket_channel -> 'a) () =
  let client_fun = server_fun_of_seqpacket_protocol ?max_input_size protocol in
  unix_domain_client ~seqpacket:() ~filename client_fun

(* stream - unix *)
let stream_unix_domain_client ?max_input_size ~filename ~(protocol:stream_channel -> 'a) () =
  let client_fun = server_fun_of_stream_protocol ?max_input_size protocol in
  unix_domain_client ~filename client_fun

(* stream - inet (v4 or v6) *)
let stream_inet_domain_client ?max_input_size ~ipv4_or_v6 ~port ~(protocol:stream_channel -> 'a) () =
  let client_fun = server_fun_of_stream_protocol ?max_input_size protocol in
  inet_domain_client ~ipv4_or_v6 ~port client_fun

(* datagram - unix *)
let datagram_unix_domain_client ?max_input_size ~filename
  ~(bootstrap : stream_channel   -> datagram_channel)
  ~(protocol  : datagram_channel -> 'a)
  () =
  let protocol_composition =
    fun stream_channel ->
      let datagram_channel = bootstrap stream_channel in
      (try stream_channel#shutdown () with _ -> ());
      let result = protocol datagram_channel in
      (datagram_channel#shutdown ~receive:() ());
      result
  in
  let client_fun = server_fun_of_stream_protocol ?max_input_size protocol_composition in
  unix_domain_client ~filename client_fun


let echo_client ~filename () =
  let pr = Printf.kfprintf flush stderr in
  let bootstrap s =
    let (fd0, sockaddr0, socketfile0) = make_derived_unix_datagram_socket ~socketfile:filename () in
    (s#send socketfile0);
    let socketfile1 = s#receive in
    let sockaddr1 = Unix.ADDR_UNIX socketfile1 in
    new datagram_channel ~fd0 ~sockaddr1 ()
  in
  let rec protocol ch =
    pr "Enter the text to send: ";
    let x = input_line stdin in
    (ch#send x);
    let y = ch#receive in
    (if x=y then (pr "Echo received, ok.\n") else (pr "Bad echo!!!!\n"));
    if y="quit" then (pr "client: QUIT!!!!\n") else protocol ch
  in
  datagram_unix_domain_client ~bootstrap ~protocol ~filename ()
