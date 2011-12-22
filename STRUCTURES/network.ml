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
    | Some () -> Unix.SOCK_SEQPACKET
  in
  let domain = Unix.domain_of_sockaddr sockaddr in
  let listen_socket = Unix.socket domain socket_type 0 in
  (* listen_socket initialization *)
  let () =
    Unix.setsockopt listen_socket Unix.SO_REUSEADDR true;
    Unix.bind listen_socket sockaddr;
    Unix.listen listen_socket max_pending_requests
  in
  let process_forking_loop () =
    let process_tutor =
      match process_tutor with
      | None   -> (function pid -> ignore (waitpid_non_intr pid))
      | Some f -> (function pid -> let () = f ~pid in ignore (waitpid_non_intr pid))
    in
    while true do
      let (service_socket, _) = accept_non_intr listen_socket in
      match Unix.fork () with
      |	0 ->
          (* The child here: *)
          begin
	    Unix.close listen_socket;
	    (try Unix.set_close_on_exec service_socket with Invalid_argument _ -> ());
	    let () = server_fun service_socket in
	    exit 0
	  end
      | child_id ->
          (* The father here: it creates a process-tutor thread per child: *)
          begin
            Unix.close service_socket;
            ignore (Thread.create process_tutor child_id)
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
    match only_threads with
    | None    -> process_forking_loop ()
    | Some () -> thread_forking_loop ()
  in
  let server_thread = Thread.create forking_loop () in
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
          Unix.mkdir result 0o777 (*0o600*);
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
class channel ?(max_input_size=1514) ?seqpacket fd =
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

end (* class channel *)

class stream_channel ?max_input_size fd =
  let in_channel  = Unix.in_channel_of_descr  fd in
  let out_channel = Unix.out_channel_of_descr fd in
  object
    inherit channel ?max_input_size fd

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
    inherit channel ?max_input_size ~seqpacket:() fd
  end (* class seqpacket_channel *)

type protocol          = channel -> unit
type stream_protocol   = stream_channel -> unit
type seqpacket_protocol = seqpacket_channel-> unit

let server_fun_of_protocol ?max_input_size ?seqpacket protocol =
  function fd -> 
    let channel = new channel ?max_input_size ?seqpacket fd in
    let () = protocol channel in
    (try channel#shutdown () with _ -> ())

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

(* seqpacket - inet *)
let seqpacket_inet_domain_server ?max_pending_requests ?max_input_size ?process_tutor ?only_threads ?ipv4 ~port ~(protocol:seqpacket_channel -> unit) () =
  let server_fun = server_fun_of_seqpacket_protocol ?max_input_size protocol in
  inet_domain_server ?max_pending_requests ~seqpacket:() ?process_tutor ?only_threads ?ipv4 ~port server_fun

(* stream - inet *)
let stream_inet_domain_server ?max_pending_requests ?max_input_size ?process_tutor ?only_threads ?ipv4 ~port ~(protocol:stream_channel -> unit) () =
  let server_fun = server_fun_of_stream_protocol ?max_input_size protocol in
  inet_domain_server ?max_pending_requests ?process_tutor ?only_threads ?ipv4 ~port server_fun

(* seqpacket - inet6 *)
let seqpacket_inet6_domain_server ?max_pending_requests ?max_input_size ?process_tutor ?only_threads ?ipv6 ~port ~(protocol:seqpacket_channel -> unit) () =
  let server_fun = server_fun_of_seqpacket_protocol ?max_input_size protocol in
  inet6_domain_server ?max_pending_requests ~seqpacket:() ?process_tutor ?only_threads ?ipv6 ~port server_fun

(* stream - inet6 *)
let stream_inet6_domain_server ?max_pending_requests ?max_input_size ?process_tutor ?only_threads ?ipv6 ~port ~(protocol:stream_channel -> unit) () =
  let server_fun = server_fun_of_stream_protocol ?max_input_size protocol in
  inet6_domain_server ?max_pending_requests ?process_tutor ?only_threads ?ipv6 ~port server_fun

let client ?seqpacket client_fun sockaddr =
  let socket_type =
    match seqpacket with
    | None    -> Unix.SOCK_STREAM
    | Some () -> Unix.SOCK_SEQPACKET
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

let inet_domain_client ?seqpacket ~ipv4_or_v6 ~port client_fun =
  let ipv4_or_v6 = Unix.inet_addr_of_string ipv4_or_v6 in
  let sockaddr = Unix.ADDR_INET (ipv4_or_v6, port) in
  client ?seqpacket client_fun sockaddr

(* seqpacket - unix *)
let seqpacket_unix_domain_client ?max_input_size ~filename ~(protocol:seqpacket_channel -> 'a) () =
  let client_fun = server_fun_of_seqpacket_protocol ?max_input_size protocol in
  unix_domain_client ~seqpacket:() ~filename client_fun

(* stream - unix *)
let stream_unix_domain_client ?max_input_size ~filename ~(protocol:stream_channel -> 'a) () =
  let client_fun = server_fun_of_stream_protocol ?max_input_size protocol in
  unix_domain_client ~filename client_fun

(* seqpacket - inet (v4 or v6) *)
let seqpacket_inet_domain_client ?max_input_size ~ipv4_or_v6 ~port ~(protocol:seqpacket_channel -> 'a) () =
  let client_fun = server_fun_of_seqpacket_protocol ?max_input_size protocol in
  inet_domain_client ~seqpacket:() ~ipv4_or_v6 ~port client_fun

(* stream - inet (v4 or v6) *)
let stream_inet_domain_client ?max_input_size ~ipv4_or_v6 ~port ~(protocol:stream_channel -> 'a) () =
  let client_fun = server_fun_of_stream_protocol ?max_input_size protocol in
  inet_domain_client ~ipv4_or_v6 ~port client_fun

