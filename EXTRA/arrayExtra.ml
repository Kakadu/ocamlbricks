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

(** Equivalent to the standard [Array.of_list] but the list is not scanned twice. The function raises [Invalid_argument]
    if the real length of the list differs from the announced. *)
let of_known_length_list ?(reversing=false) len = function
 | []    -> [||]
 | x::xs -> let a = Array.create len x in
     if reversing then
      (let rec fill i = function
       | [] -> (if i=(-1) then a else invalid_arg "unexpected list length (overstated size)")
       | x::xs -> (try a.(i) <- x with _ -> invalid_arg "unexpected list length (understated size)"); fill (i-1) xs 
       in fill (len-2) xs)
     else
      (let rec fill i = function
      | [] -> (if i=len then a else invalid_arg "unexpected list length (overstated size)")
      | x::xs -> (try a.(i) <- x with _ -> invalid_arg "unexpected list length (understated size)"); fill (i+1) xs 
      in fill 1 xs) 

(** Similar to the standard [List.for_all], implemented directly, i.e. without conversion. *)
let for_all p s =
 let l = Array.length s in
 let rec loop i =
  if i>=l then true else
  (p i s.(i)) && loop (i+1)
 in loop 0

(** Similar to the standard [List.exists], implemented directly, i.e. without conversion. *)
let exists p s =
 let l = Array.length s in
 let rec loop i =
  if i>=l then false else
  (p i s.(i)) || loop (i+1)
 in loop 0

(** As the function [exists], but provides the index that verifies the predicate. *)
let lexists p s =
 let l = Array.length s in
 let rec loop i =
  if i>=l then None else
  if (p i s.(i)) then (Some i) else loop (i+1)
 in loop 0

(** As the function [lexists], but searching from the right side. *)
let rexists p s =
 let l = Array.length s in
 let rec loop i =
  if i<0 then None else
  if (p i s.(i)) then (Some i) else loop (i-1)
 in loop (l-1)


let dichotomic_search v x =
 let rec loop a b =
   if a=b then a else
   let i = (a+b)/2 in
   if x > v.(i) then loop (i+1) b else
   if (i>0) && (v.(i-1) >= x) then loop a (i-1) else
   i
 in
 let i = loop 0 ((Array.length v)-1) in
 ((v.(i) = x),i)

let dichotomic_insert v x =
 let l = Array.length v in
 let last_index = l-1 in
 match dichotomic_search v x with
 | true, _ -> v
 | false, index when (index = last_index) ->
     Array.init (l+1) (fun i -> try v.(i) with _ -> x)
 | false, index ->
    Array.init
      (l+1)
      (fun i -> match compare i index with (-1) -> v.(i) | 0 -> x | _ -> v.(i-1))
 
(** Tools for matrices (arrays of arrays). *)
module Matrix = struct

 type 'a t = 'a array array

 (** [init m n f] returns a fresh [m] x [n] matrix with element [(i,j)] initialized to the result of [(f i j)].  *)
 let init m n f =
  Array.init m (fun i -> Array.init n (f i))

 (** Make a matrix from a list of lists. *)
 let of_list ll =
  if ll = [] then [||] else
  let rows = List.length ll in
  Array.init rows (fun row -> Array.of_list (List.nth ll row))

 (** Make a list of lists from a matrix. *)
 let to_list aa =
  let al = Array.map Array.to_list aa in
  Array.to_list al

 (** Transpose the matrix. *)
 let transpose aa =
  let m = Array.length aa     in
  let n = Array.length aa.(0) in
  if (for_all (fun i a -> (Array.length a) = n) aa)
  then init n m (fun i j -> aa.(j).(i))
  else invalid_arg "transpose: the argument is not a matrix."

end

