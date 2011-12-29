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


class stream_channel :
  ?max_input_size:int ->
  Unix.file_descr ->
  object
    method send    : string -> unit
    method receive : string
    method peek    : string

    method input_char       : char
    method input_line       : string
    method input_byte       : int
    method input_binary_int : int
    method input_value      : 'a

    method output_char       : char -> unit
    method output_string     : string -> unit
    method output_byte       : int -> unit
    method output_binary_int : int -> unit
    method output_value      : 'b -> unit

    method shutdown : ?receive:unit -> ?send:unit -> unit -> unit
  end

class seqpacket_channel :
  ?max_input_size:int ->
  Unix.file_descr ->
  object
    method send    : string -> unit
    method receive : string
    method peek    : string

    method shutdown : ?receive:unit -> ?send:unit -> unit -> unit
  end

class datagram_channel :
  ?max_input_size:int ->
  ?socketfile0:string ->
  socketfile1:string ->
  unit ->
  object
 
    method send    : string -> unit
    method receive : string
    method peek    : string

    method shutdown : ?receive:unit -> ?send:unit -> unit -> unit

    method socketfile0 : string
    method socketfile1 : string
    method sockaddr0 : Unix.sockaddr
    method sockaddr1 : Unix.sockaddr
    method fd0 : Unix.file_descr
end

type stream_protocol    = stream_channel    -> unit
type seqpacket_protocol = seqpacket_channel -> unit
type datagram_protocol  = (stream_channel -> datagram_channel) * (datagram_channel -> unit)

(** {2 Seqpacket Unix Domain } *)

val seqpacket_unix_domain_server :
  ?max_pending_requests:int ->
  ?max_input_size:int ->
  ?process_tutor:(pid:int -> unit) ->
  ?only_threads:unit ->
  ?filename:string ->
  protocol:(seqpacket_channel -> unit) ->
  unit -> Thread.t * string

val seqpacket_unix_domain_client :
  ?max_input_size:int ->
  filename:string ->
  protocol:(seqpacket_channel -> 'a) ->
  unit -> 'a

(** {2 Stream Unix Domain } *)

val stream_unix_domain_server :
  ?max_pending_requests:int ->
  ?max_input_size:int ->
  ?process_tutor:(pid:int -> unit) ->
  ?only_threads:unit ->
  ?filename:string ->
  protocol:(stream_channel -> unit) ->
  unit -> Thread.t * string

val stream_unix_domain_client :
  ?max_input_size:int ->
  filename:string ->
  protocol:(stream_channel -> 'a) ->
  unit -> 'a

(** {2 Stream Internet Domain } *)

val stream_inet_domain_server :
  ?max_pending_requests:int ->
  ?max_input_size:int ->
  ?process_tutor:(pid:int -> unit) ->
  ?only_threads:unit ->
  ?ipv4:string ->
  port:int -> protocol:(stream_channel -> unit) ->
  unit -> Thread.t * string

val stream_inet6_domain_server :
  ?max_pending_requests:int ->
  ?max_input_size:int ->
  ?process_tutor:(pid:int -> unit) ->
  ?only_threads:unit ->
  ?ipv6:string ->
  port:int ->
  protocol:(stream_channel -> unit) ->
  unit -> Thread.t * string

val stream_inet_domain_client :
  ?max_input_size:int ->
  ipv4_or_v6:string ->
  port:int ->
  protocol:(stream_channel -> 'a) ->
  unit -> 'a

(* datagram - unix *)
val datagram_unix_domain_server :
  ?max_pending_requests:int ->
  ?max_input_size:int ->
  ?process_tutor:(pid:int -> unit) ->
  ?only_threads:unit ->
  ?filename:string ->
  bootstrap:(stream_channel -> datagram_channel) ->
  protocol:(datagram_channel -> unit) ->
  unit -> Thread.t * string

val echo_server : unit -> Thread.t * string
val echo_client : unit -> unit

val datagram_unix_domain_client :
  ?max_input_size:int ->
  filename:string ->
  bootstrap:(stream_channel -> datagram_channel) ->
  protocol:(datagram_channel -> 'a) ->
  unit -> 'a

