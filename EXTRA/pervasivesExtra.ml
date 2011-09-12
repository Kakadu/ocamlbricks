(* This file is part of our reusable OCaml BRICKS library
   Copyright (C) 2007  Luca Saiu

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

(* Authors:
 * - Luca Saiu: initial version
 * - Jean-Vincent Loddo: functors for printers
 *)

(* Do not remove the following comment: it's an ocamldoc workaround. *)
(** *)

(** Round the float. By default the number of decimals is set to 3. *)
let round ?(decimals=3) x =
  let k = 10. ** (float_of_int decimals) in
  (floor (x *. k +. 0.5)) /. k

(** For-based folder using float numbers. *)
let for_float ?backward ~min ~max ~step f acc =
  let tollerance = step /. 2. in
  match backward with
  | None ->
      let max = max +. tollerance in
      let rec loop acc x =
	if x > max then acc else loop (f acc x) (x+.step)
      in
      loop acc min
  | Some () ->
      let min = min -. tollerance in
      let rec loop acc x =
	if x < min then acc else loop (f acc x) (x-.step)
      in
      loop acc min

(** For-based folder using int numbers. *)
let for_int ?backward ?(step=1) ~min ~max f acc =
  match backward with
  | None ->
      let rec loop acc x =
	if x > max then acc else loop (f acc x) (x+step)
      in
      loop acc min
  | Some () ->
      let rec loop acc x =
	if x < min then acc else loop (f acc x) (x-step)
      in
      loop acc min

let print_char c     = Pervasives.print_char c;     flush stdout
let print_string s   = Pervasives.print_string s;   flush stdout
let print_int i      = Pervasives.print_int i;      flush stdout
let print_float f    = Pervasives.print_float f;    flush stdout
let print_endline s  = Pervasives.print_endline s;  flush stdout
let print_newline () = Pervasives.print_newline (); flush stdout

(** Make standard printers for a non-polymorphic type (0-arity type). *)
module Printers0 (M:sig type t val string_of : t -> string end) = struct
   include M
   let print              x = (Printf.printf  "%s"   (M.string_of x))
   let prerr              x = (Printf.eprintf "%s"   (M.string_of x))
   let print_endline      x = (Printf.printf  "%s\n" (M.string_of x)); (flush stdout)
   let prerr_endline      x = (Printf.eprintf "%s\n" (M.string_of x)); (flush stderr)
   let fprintf outch frmt x = (Printf.fprintf outch  frmt (M.string_of x))
   let eprintf       frmt x = (Printf.fprintf stderr frmt (M.string_of x))
   let printf        frmt x = (Printf.printf         frmt (M.string_of x))
   let sprintf       frmt x = (Printf.sprintf        frmt (M.string_of x))
  end

(** Make standard printers for a polymorphic type (1-arity type). *)
module Printers1 (M:sig type 'a t val string_of : ('a->string) -> 'a t -> string end) = struct
   include M
   let print         string_of_alpha x = (Printf.printf  "%s"   (M.string_of string_of_alpha x))
   let prerr         string_of_alpha x = (Printf.eprintf "%s"   (M.string_of string_of_alpha x))
   let print_endline string_of_alpha x = (Printf.printf  "%s\n" (M.string_of string_of_alpha x)); (flush stdout)
   let prerr_endline string_of_alpha x = (Printf.eprintf "%s\n" (M.string_of string_of_alpha x)); (flush stderr)
   let fprintf       string_of_alpha outch frmt x = (Printf.fprintf outch  frmt (M.string_of string_of_alpha x))
   let eprintf       string_of_alpha       frmt x = (Printf.fprintf stderr frmt (M.string_of string_of_alpha x))
   let printf        string_of_alpha       frmt x = (Printf.printf         frmt (M.string_of string_of_alpha x))
   let sprintf       string_of_alpha       frmt x = (Printf.sprintf        frmt (M.string_of string_of_alpha x))
  end
