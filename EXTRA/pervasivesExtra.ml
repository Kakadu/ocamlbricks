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


let for_float ?break ?backward ~min ~max ~step f acc =
  let tollerance = step /. 2. in
  match backward, break with
  | None, None ->
      let max = max +. tollerance in
      let rec loop acc x =
	if x > max then acc else loop (f acc x) (x+.step)
      in
      loop acc min
  | None, Some break ->
      let max = max +. tollerance in
      let rec loop acc x =
	if x > max || (break acc x) then acc else loop (f acc x) (x+.step)
      in
      loop acc min
  | Some (), None ->
      let min = min -. tollerance in
      let rec loop acc x =
	if x < min then acc else loop (f acc x) (x-.step)
      in
      loop acc min
  | Some (), Some break ->
      let min = min -. tollerance in
      let rec loop acc x =
	if x < min || (break acc x) then acc else loop (f acc x) (x-.step)
      in
      loop acc min


(** For-based folder using int numbers. *)
let for_int ?break ?backward ?(step=1) ~min ~max f acc =
  match backward, break with
  | None, None ->
      let rec loop acc x =
	if x > max then acc else loop (f acc x) (x+step)
      in
      loop acc min
  | None, Some break ->
      let rec loop acc x =
	if x > max || (break acc x) then acc else loop (f acc x) (x+step)
      in
      loop acc min
  | Some (), None ->
      let rec loop acc x =
	if x < min then acc else loop (f acc x) (x-step)
      in
      loop acc min
  | Some (), Some break ->
      let rec loop acc x =
	if x < min || (break acc x) then acc else loop (f acc x) (x-step)
      in
      loop acc min


let get_first_line_of_file filename =
  try
    let ch = open_in filename in
    let line = input_line ch in
    let () = close_in ch in
    Some line
  with _ -> None


let get_first_lines_of_file filename n =
  try
    let ch = open_in filename in
    let rec loop k acc =
      if k=0 then List.rev acc else
      try
        let line = input_line ch in
        loop (k-1) (line::acc)
      with _ -> List.rev acc
    in
    let result = loop n [] in
    let () = close_in ch in
    result
  with _ -> []
