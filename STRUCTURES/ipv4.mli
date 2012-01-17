(* This file is part of Marionnet, a virtual network laboratory
   Copyright (C) 2009 Jean-Vincent Loddo

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

(** IPv4 parsing and printing. *)

type byte    = int
type ipv4    = byte * byte * byte * byte
type cidr    = byte
type netmask = byte * byte * byte * byte

type config          = ipv4 * cidr
type verbose_config  = ipv4 * netmask

(** {2 Netmask <-> CIDR} *)

val netmask_of_cidr  : cidr -> netmask
val cidr_of_netmask  : netmask -> cidr

(** {2 Checking} *)

val is_valid_ipv4           : ipv4           -> bool
val is_valid_cidr           : cidr           -> bool
val is_valid_netmask        : netmask        -> bool
val is_valid_config         : ?strict:bool -> config -> bool
val is_valid_verbose_config : ?strict:bool -> verbose_config -> bool

(** {2 Parsing} *)

val ipv4_of_string : string -> ipv4
val string_of_ipv4 : ?cidr:cidr -> ipv4 -> string

val config_of_string            : ?strict:bool -> string -> config
val verbose_config_of_strings   : ?strict:bool -> string -> string -> verbose_config
val netmask_with_cidr_of_string : string -> netmask * cidr
val netmask_of_string           : string -> netmask

type ipcalc_result =
< ip : ipv4;
  cidr      : cidr;
  netmask   : ipv4;
  network   : ipv4;
  broadcast : ipv4;
  hostmin   : ipv4;
  hostmax   : ipv4;
  hosts     : int;
  print     : unit;

  to_string : <
      ip        : string;
      netmask   : string;
      network   : string;
      broadcast : string;
      hostmax   : string;
      hostmin   : string;
      >;

  contains : ipv4 -> bool;
  >

val ipcalc : ipv4 -> cidr -> ipcalc_result

(** {2 String checking} *)

module String : sig
 val is_valid_ipv4           : string -> bool
 val is_valid_cidr           : string -> bool
 val is_valid_netmask        : string -> bool
 val is_valid_config         : ?strict:bool -> string -> bool
 val is_valid_verbose_config : ?strict:bool -> string -> string -> bool
 val ipcalc : config:string ->
  < ip        : string;
    cidr      : string;
    netmask   : string;
    network   : string;
    broadcast : string;
    hostmax   : string;
    hostmin   : string;
    contains  : string -> bool;
    print     : unit;
    >
end


  