(* This file is part of our reusable OCaml BRICKS library
   Copyright (C) 2007  Jean-Vincent Loddo

   Trivial change in 2008 by Luca Saiu

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

open Sugar


(** Similar to the standard [List.hd], but retrieve the list of first elements (by default [n=1] as in [List.hd]).
    Thus, the result is a list. *)
let rec head ?(n:int=1) (l:'a list) : ('a list) =
  if n<=0 then [] else let n = (n-1) in 
  match l with 
  | []   -> []
  | x::r -> x::(head ~n r) 

(** Similar to the standard [List.tl], but the tail is extracted from the given index
    (by default [i=1] as in [List.tl]) *)
let rec tail ?(i:int=1) (l:'a list) =
  if (i=0) then l else tail ~i:(i-1) (List.tl l)

(** Substract the second argument from the first *)
let substract = fun u d -> let p=(fun y -> not (List.mem y d)) in (List.filter p u)

(** [subset a b] check if [a] is a subset of [b], i.e. if all elements of a belong to b. *)
let subset a b = List.for_all (fun x->(List.mem x b)) a

(** [eqset a b] check if a and b represent the same set of values. *)
let eqset a b = (subset a b) && (subset b a)

(** Intersection of list: AvB=A\(A\B) . *)
let intersection a b = substract a (substract a b)

(** Shortcut for [List.iter] with arguments in the opposite order: before the list, then the action to perfom. *)
let foreach l f = List.iter f l

(** Returns a list with no duplicates.
    For large lists we suggest to use {!Hashset.uniq} instead. *)
let rec uniq = function
  | []   -> []
  | x::r -> if (List.mem x r) then (uniq r) else x::(uniq r) 

(** As [uniq] but with the optional argument [take_first] you can set the policy for taking elements.
    By default the policy is the opposite of [uniq], i.e. you take the first occurrence, not the last. *)
let remove_duplicates ?(take_first=true) =
 let rec loop acc = match take_first with
 | true  ->
    (function
    | []    -> acc
    | x::xs -> if (List.mem x acc) then (loop acc xs) else (loop (x::acc) xs)
    )
 | false ->
    (function
    | []    -> acc
    | x::xs -> if (List.mem x xs)  then (loop acc xs) else (loop (x::acc) xs)
    )
  in function xs -> List.rev (loop [] xs)
;;

(** [range a b] returns the list [\[a; (a+1); .. ; (b-1); b\]] containing all the values between the given limits (included) . *)
let range (a:int) (b:int) =
  let rec range a b acc = if a>b then acc else (range a (b-1) (b::acc)) in
   range a b []

(** Alias for range. *)
let interval = range

(** The list of indexes of a list. The first index is [0] as usually. *)
let indexes l = range 0 ((List.length l)-1);;

(** Consider a list as a function from indexes to its content. The function is the identity outside the indexes of the list. *)
let asFunction l = fun i -> try (List.nth l i) with _ -> i;;

(** Considering a list as a record and select some fields (indexes). Example:

{[# select ["aaa";"bbb";"ccc"] [1;2;0;1];; 
  : string list = ["bbb"; "ccc"; "aaa"; "bbb"]
]}
     *)
let select (l:'a list) (fieldlist:int list) =
 let a = Array.of_list l in
 let rec loop a = function 
 | []    -> []
 | f::fl -> (Array.get a f)::(loop a fl)
 in loop a fieldlist

(** Remove the element with the given index. *)
let rmindex l i =
 let rec rmindex acc = function
  | (0,x::xs) -> List.append (List.rev acc) xs
  | (i,x::xs) -> rmindex (x::acc) (i-1,xs)    
  | (_,[])    -> failwith "rmindex: index out of bounds" in
 rmindex [] (i,l)

(** Search for the first index of an element satisfying a property. *)
let indexSuchThat (pred:'a->bool) (l:'a list) : (int option) =
  let rec indexOf pred l i = (match l with 
  | []                  -> None
  | y::r when (pred y)  -> Some i
  | y::r                -> indexOf pred r (i+1) ) 
  in indexOf pred l 0

(** Search for the first index of an element in a list *)
let indexOf (x:'a) (l:'a list) : (int option) = indexSuchThat ((=)x) l

(** Alias for [indexOf]. *)
let firstIndexOf = indexOf

(** Search for the last index of an element in a list *)
let lastIndexOf x l =
  let n = List.length l in
  match indexOf x (List.rev l) with
  | None   -> None
  | Some i -> Some (n-1-i)

(** Returns a permutation of the list. *)
let rec shuffle l = if l = [] then [] else
  let i = Random.int (List.length l) in
  let l'  = (rmindex l i) in
  (List.nth l i)::(shuffle l')

(** List permutation. The first argument is the function [f] that represents the permutation
    (we suppose that this function will be a bijection w.r.t. the set of indexes of the given list).
    In other words [permute f l] is the list [\[(f 0) ; (f 1) ; (f 2) ; ... \] ].  *)
let permute f l = List.map (fun i -> List.nth l (f i)) (indexes l)

(** Return a random permutation function for the given list. *)
let shuffler l = l => (indexes || shuffle || asFunction )

(** Return a random list of indexes for the given list. *)
let shuffleIndexes l = l => (indexes || shuffle)

(** The {e folding} of lists is simply a [List.fold_left] specialization:

     - the first element is the {b head} of the list 
     - the folding is performed on the {b tail} of the list. 

   This function is adequate for most common cases. Use the module [Big] when 
   maximum generality is requested. *)
let big f = function
  | []   -> failwith "big"
  | [x]  -> x
  | x::r -> List.fold_left f x r  

(** {b Common foldings} *)

(** The polymorphic maximum of a list. *)
let max (l:'a list) : 'a = big max l;;

(** The polymorphic minimum of a list. *)
let min (l:'a list) : 'a = big min l;;

(** Transpose the matrix (list of lists). Raise [Invalid_argument "transpose"] if the argument is not a matrix.
{b Example}:
{[# ListExtra.transpose [[1;2;3]; [4;5;6]; [7;8;9]];;
  : int list list = [[1; 4; 7]; [2; 5; 8]; [3; 6; 9]]
]}*)
let transpose ll =
 let aa  = ArrayExtra.Matrix.of_list ll    in
 let aa' = ArrayExtra.Matrix.transpose  aa in
 let ll' = ArrayExtra.Matrix.to_list aa'   in
 ll'
 


