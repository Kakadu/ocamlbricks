(* This file is part of our reusable OCaml BRICKS library
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

(* Do not remove the following comment: it's an ocamldoc workaround. *)
(** *)

type byte    = int
type ipv4    = byte * byte * byte * byte
type cidr    = byte
type netmask = byte * byte * byte * byte

type config          = ipv4 * cidr
type verbose_config  = ipv4 * netmask


let prettify_scanf_exception caller = function
 | Scanf.Scan_failure msg -> failwith (caller^": "^msg)
 | End_of_file            -> failwith (caller^": scanf: incomplete matching")
 | e                      -> raise e

(* ************************************
         ipv4 checking and parsing
   ************************************ *)

(** A valid ipv4 has each byte in the range [0..255]. *)
let is_valid_ipv4 (b1,b2,b3,b4) =
 List.for_all (fun x->(x>=0) && (x<=255)) [b1; b2; b3; b4]

(* Do nothing or raise an exception. *)
let check_ipv4 ?caller ?parsed_string ((b1,b2,b3,b4) as ip) =
 begin
  if is_valid_ipv4 ip
  then ()
  else (* Create the error message. *)
  let caller = match caller with Some x -> x | None -> "check_ipv4" in
  let error  = List.find (fun x->not ((x>=0) && (x<=255))) [b1; b2; b3; b4] in
  let msg = match parsed_string with
   | None       -> Printf.sprintf "%s: not a valid ip: integer %d out of the range [0..255]"    caller error
   | (Some str) -> Printf.sprintf "%s: ill-formed ip %s: integer %d out of the range [0..255]" caller str error
  in failwith msg
 end

(** Example: ["192.168.1.42" -> (192,168,1,42)] *)
let ipv4_of_string str =
 try
  let result = Scanf.sscanf str "%i.%i.%i.%i" (fun b1 b2 b3 b4 -> (b1, b2, b3, b4)) in
  (check_ipv4 ~caller:"ipv4_of_string" ~parsed_string:str result);
  result
 with
  e -> prettify_scanf_exception "ipv4_of_string" e

(** Example: ["192.168.1.42" -> (192,168,1,42)] *)
let string_of_ipv4 ?cidr (b1, b2, b3, b4) =
 match cidr with
 | None    -> Printf.sprintf "%i.%i.%i.%i" b1 b2 b3 b4
 | Some b5 -> Printf.sprintf "%i.%i.%i.%i/%i" b1 b2 b3 b4 b5

(* ************************************
     cidr/netmask checking and parsing
   ************************************ *)

(** A valid CIDR is in the range [0..32]. *)
let is_valid_cidr cidr = (cidr>=0) && (cidr<=32)

let check_cidr ?caller cidr =
 begin
  if is_valid_cidr cidr
    then ()
    else (* Create the error message. *)
    let caller = match caller with Some x -> x | None -> "check_cidr" in
    failwith (Printf.sprintf "%s: CIDR netmask %d out of the range [0..32]" caller cidr)
 end

(* Return a cidr in the range [0..8] from a byte. Example: 128 -> 1. *)
let cidr_of_byte ?caller = function
 | 0   -> 0 | 128 -> 1 | 192 -> 2 | 224 -> 3 | 240 -> 4 | 248 -> 5 | 252 -> 6 | 254 -> 7 | 255 -> 8
 | x   ->
    let caller = match caller with Some x -> x | None -> "cidr_of_byte" in
    failwith (Printf.sprintf "%s: %d doesn't represent a valid CIDR netmask byte (admissible values are 0,128,192,224,240,248,252,254,255)" caller x)

(** A valid netmask starts with zero or more bytes equal to [255], followed by a byte in the set [\{0,128,192,224,240,248,252,254,255\}],
    followed by some bytes equal to [0]. *)
let is_valid_netmask (n1,n2,n3,n4) =
 try
  let cs = List.map cidr_of_byte [n1;n2;n3;n4] in
  match cs with
  | [_;0;0;0] | [8;_;0;0] | [8;8;_;0] | [8;8;8;_] -> true
  | _ -> false
 with _ -> false

(* Return the CIDR or raise an exception.
   Examples:
    (255,255,248,0)   -> 21
    (255,255,248,255) -> exception
    (255,255,251,0)   -> exception. *)
let check_netmask ?caller (n1,n2,n3,n4) =
 begin
  let cs = List.map (cidr_of_byte ?caller) [n1;n2;n3;n4] in
  match cs with
  | [_;0;0;0] | [8;_;0;0] | [8;8;_;0] | [8;8;8;_] -> (List.fold_left (+) 0 cs)
  | _ -> (* Create the error message. *)
  let caller = match caller with Some x -> x | None -> "check_netmask" in
  let msg = (Printf.sprintf "%s: ill-formed netmask %i.%i.%i.%i: bytes are not in the right order" caller n1 n2 n3 n4) in
  failwith msg
 end

(** Example: ["255.255.248.0" -> ((255, 255, 248, 0), 21)] *)
let netmask_with_cidr_of_string str =
 try
  let extended = Scanf.sscanf str "%i.%i.%i.%i" (fun b1 b2 b3 b4 -> (b1, b2, b3, b4)) in
  let cidr = (check_netmask ~caller:"netmask_with_cidr_of_string" extended) in
  (extended,cidr)
 with
  e -> prettify_scanf_exception "netmask_with_cidr_of_string" e


(** Example: ["255.255.248.0" -> (255, 255, 248, 0)] *)
let netmask_of_string str = fst (netmask_with_cidr_of_string str)

(* Return a byte from a cidr in the range [0..8]. Example: 1 -> 128 *)
let byte_of_cidr x =
 let rec loop c = if c=0. then 0. else (loop (c -. 1.)) +. (2. ** (8. -. c))
 in int_of_float (loop (float_of_int x))
 ;;

(* Return a cidr in the range [0..8] from a byte. Example: 128 -> 1. *)
let cidr_of_byte caller = function
 | 0   -> 0 | 128 -> 1 | 192 -> 2 | 224 -> 3 | 240 -> 4 | 248 -> 5 | 252 -> 6 | 254 -> 7 | 255 -> 8
 | x   -> failwith (Printf.sprintf "%s: %d doesn't represent a valid CIDR netmask byte (admissible values are 0,128,192,224,240,248,252,254,255)" caller x)


(* ************************************
      cidr <-> netmask conversions
   ************************************ *)

(** Example: [23 -> (255,255,254,0)] *)
let netmask_of_cidr x =
 if not ((x>=0) && (x<=32))
   then failwith (Printf.sprintf "netmask_of_cidr: CIDR netmask %d out of the range [0..32]" x)
   else
   let rec loop i acc c =
    if i > 4 then acc else
    if c>8 then (loop (i+1) (255::acc) (c-8))
           else (loop (i+1) ((byte_of_cidr c)::acc) 0)
   in
   match (loop 1 [] x) with
   | [n4;n3;n2;n1] -> (n1,n2,n3,n4)
   | _ -> assert false


(** Example: [(255,255,254,0) -> 23] *)
let cidr_of_netmask ((n1,n2,n3,n4) as netmask) =
 check_netmask ~caller:"cidr_of_netmask" netmask


(* ********************************************
     config (ip/netmask) checking and parsing
   ******************************************** *)

(*  Check individual validity of ip and cidr, then verify that the ip is neither
    the first possible (network address) nor the last possible (broadcast address). *)
let is_coherent_config ?(strict=false) (i1,i2,i3,i4) cidr =
 begin
  if (cidr>=31)
   then
    (* In this case, the ip is necessarily the unique possible (cidr=32),
       or either the first or the last possible (cidr=31).
       Thus, I accept this configuration iff I am not strict. *)
    (not strict)
  else
   let (n1,n2,n3,n4) = netmask_of_cidr cidr in
   let (a1,a2,a3,a4) = (i1  land n1, i2  land n2, i3  land n3, i4  land n4) in (* network address *)
   let (l1,l2,l3,l4) = (255 lxor n1, 255 lxor n2, 255 lxor n3, 255 lxor n4) in (* network capacity *)
   let (c1,c2,c3,c4) = (i1  land l1, i2  land l2, i3  land l3, i4  land l4) in (* relative address in the network. *)
       ((a1 <> i1) or (a2 <> i2) or (a3 <> i3) or (a4 <> i4))  (* not the first (network) address *)
   &&  ((c1 <  l1) or (c2 <  l2) or (c3 <  l3) or (c4 <  l4))  (* not the last address *)
 end

(** A configuration [(ip,cidr)] is valid if both [ip] and [cidr] are valid and if the [ip] is neither
    the first possible address nor the last possible address
    in the network defined by [cidr] (reserved for network and broadcast addresses).
    The optional parameter [?strict=false] determines the
    policy to adopt when the network capacity is lesser than 4 (this happen when [cidr>=31]):
    if we are {e strict} we consider this value as a wrong configuration. *)
let is_valid_config ?strict (((i1,i2,i3,i4) as ip),cidr) =
 (is_valid_ipv4 ip) && (is_valid_cidr cidr) && (is_coherent_config ?strict ip cidr)

(** As [is_valid_config] but using a verbose netmask definition instead of the CIDR notation. *)
let is_valid_verbose_config ?strict (ip,netmask) =
 (is_valid_netmask netmask) &&
 begin
  let cidr = cidr_of_netmask netmask in
  is_valid_config ?strict (ip,cidr)
 end

let check_config ?strict ?caller ((i1,i2,i3,i4) as ip) cidr =
 begin
  if is_coherent_config ?strict ip cidr
   then ()
   else
    let caller = match caller with Some x -> x | None -> "check_config" in
    failwith (Printf.sprintf "%s: %d.%d.%d.%d/%d is the first or the last address in the network" caller i1 i2 i3 i4 cidr)
 end

(** Example: ["192.168.1.42/24" -> ((192,168,1,42),24)] *)
let config_of_string ?strict str =
 try
  let (ip,cidr) = Scanf.sscanf str "%i.%i.%i.%i/%i" (fun b1 b2 b3 b4 b5 -> ((b1, b2, b3, b4),b5)) in
  (check_ipv4   ~caller:"config_of_string" ~parsed_string:str ip);
  (check_cidr   ~caller:"config_of_string" cidr);
  (check_config ~caller:"config_of_string" ?strict ip cidr);
  (ip,cidr)
 with
  e -> prettify_scanf_exception "config_of_string" e

(** Example: ["192.168.1.42" "255.255.255.128" -> ((192,168,1,42),25)] *)
let verbose_config_of_strings ?(strict=false) str_ip str_netmask =
 let ip = ipv4_of_string str_ip in
 let (netmask,cidr) = netmask_with_cidr_of_string str_netmask in
 (check_config ~caller:"verbose_config_of_strings" ~strict ip cidr);
 (ip,netmask)

(* ********************************************
          String parsing then checking
   ******************************************** *)

module String = struct

 let try_action_or_false    (f:'a->'b)   = function x -> (try (ignore (f x); true) with _ -> false)
 let try_predicate_or_false (f:'a->bool) = function x -> (try (f x) with _ -> false)

 (** Example: ["33" -> false], while: ["32" -> true]. *)
 let is_valid_cidr    = try_predicate_or_false (fun s -> is_valid_cidr (int_of_string s))

 (** Examples: ["192.168.1" -> false], while: ["192.168.1.128" -> true]. *)
 let is_valid_ipv4    = try_action_or_false ipv4_of_string

 (** Examples: ["255.255.128.128" -> false], while: ["255.255.255.128" -> true]. *)
 let is_valid_netmask = try_action_or_false netmask_with_cidr_of_string

 (** Examples: ["192.168.1.0/24" -> false], while: ["192.168.1.1/24" -> true]. *)
 let is_valid_config  ?(strict=false) = try_action_or_false (config_of_string ~strict)

 (** Examples: ["192.168.1.255" "255.255.255.252" -> false], while: ["192.168.1.254" "255.255.255.252"-> true].*)
 let is_valid_verbose_config ?(strict=false) str_ip =
  try_predicate_or_false
   (fun str_netmask ->
      let ip = ipv4_of_string str_ip in
      let netmask = netmask_of_string str_netmask in
      is_valid_verbose_config ~strict (ip,netmask))

end

