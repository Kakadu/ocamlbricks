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

let sorted_copy ?(compare=Pervasives.compare) xs = 
  let ys = (Array.copy xs) in
  (Array.sort compare ys); 
  ys

let fast_sorted_copy ?(compare=Pervasives.compare) xs = 
  let ys = (Array.copy xs) in
  (Array.fast_sort compare ys); 
  ys

(** {b Example}:
{[
# init2 3 (fun i -> (i+1,i*2)) ;;
  : int array * int array = ([|1; 2; 3|], [|0; 2; 4|])
]} *)
let init2 n f =
  if n = 0 then ([||],[||]) else
  let (x0,y0) = f 0 in
  let xs = Array.create n x0 in
  let ys = Array.create n y0 in
  for i = 1 to (n-1) do
    let (x,y) = f i in
    xs.(i) <- x;
    ys.(i) <- y;
  done;
  (xs,ys)

let split xys = init2 (Array.length xys) (fun i -> xys.(i))

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

let search p s =
 let l = Array.length s in
 let rec loop i =
  if i>=l then None else
  let x = s.(i) in 
  if (p x) then (Some x) else loop (i+1)
 in loop 0

let searchi p s =
 let l = Array.length s in
 let rec loop i =
  if i>=l then None else
  let x = s.(i) in 
  if (p x) then (Some (i,x)) else loop (i+1)
 in loop 0

let find p s =
 let l = Array.length s in
 let rec loop i =
  if i>=l then raise Not_found else
  let x = s.(i) in 
  if (p x) then x else loop (i+1)
 in loop 0

let findi p s =
 let l = Array.length s in
 let rec loop i =
  if i>=l then raise Not_found else
  let x = s.(i) in 
  if (p x) then (i,x) else loop (i+1)
 in loop 0

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
 

let for_all2 f xs ys = for_all (fun i x -> f i x ys.(i)) xs
let exists2  f xs ys = exists  (fun i x -> f i x ys.(i)) xs

let iter2  f a b = Array.iteri (fun i a -> f a b.(i)) a 
let iteri2 f a b = Array.iteri (fun i a -> f i a b.(i)) a 

let map2  f a b = Array.mapi (fun i a -> f a b.(i)) a 
let mapi2 f a b = Array.mapi (fun i a -> f i a b.(i)) a 

let fold_lefti f y0 s =
 let l = Array.length s in
 let rec loop acc i =
  if i>=l then acc else
  let acc = f i acc s.(i) in  
  loop acc (i+1)
 in loop y0 0

let fold_righti f s y0 =
 let l = Array.length s in
 let rec loop acc i =
  if i<0 then acc else
  let acc = f i s.(i) acc in  
  loop acc (i-1)
 in loop y0 (l-1)

let fold_left2  f s0 xs ys = fold_lefti  (fun i s x -> f s x ys.(i)) s0 xs
let fold_right2 f xs ys s0 = fold_righti (fun i x s -> f x ys.(i) s) xs s0

let fold_lefti2  f s0 xs ys = fold_lefti  (fun i s x -> f i s x ys.(i)) s0 xs
let fold_righti2 f xs ys s0 = fold_righti (fun i x s -> f i x ys.(i) s) xs s0

(** Similar to [List.partition] but for arrays and with many classes. 
{b Example}:
{[
# partition (fun x -> x mod 3) [|0;1;2;3;4;5;6;7;8;9|] ;; 
  : int array array = [|[|0; 3; 6; 9|]; [|1; 4; 7|]; [|2; 5; 8|]|]
]} *)
let partition =
  let errmsg = "ArrayExtra.partition: classifier must provide only non-negative integers" in
  fun f a ->
  (* f' is a dynamically type checking version of f: *)
  let f' x = (let y = f x in (if (y<0) then invalid_arg errmsg); y) in  
  let max_index = Array.fold_left (fun s x -> max s (f' x)) (-1) a in
  if max_index = -1 then [||] else
  let ls = Array.create (max_index+1) [] in
  (Array.iteri (fun i x -> let c = f x in ls.(c) <- x :: ls.(c)) a);
  let result = Array.map (fun l -> Array.of_list (List.rev l)) ls in
  result

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

