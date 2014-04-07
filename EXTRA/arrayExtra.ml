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
let combine xs ys = Array.init (Array.length xs) (fun i -> xs.(i), ys.(i))
      
let sorted_copy ?(compare=Pervasives.compare) xs =
  let ys = (Array.copy xs) in
  (Array.sort compare ys);
  ys

let fast_sorted_copy ?(compare=Pervasives.compare) xs =
  let ys = (Array.copy xs) in
  (Array.fast_sort compare ys);
  ys

(** Sort the array saving the position of each element in the original array. {b Example}:
{[ ArrayExtra.sort_saving_positions [| 6.28; 3.14; 1.41; 2.71 |] ;;
  : (int * float) array = [|(2, 1.41); (3, 2.71); (1, 3.14); (0, 6.28)|]
]} *)
let sort_saving_positions ?(compare=Pervasives.compare) (xs:'a array) : (int * 'a) array =
  let ys = Array.mapi (fun i x -> (x,i)) xs in
  let () = Array.sort (fun (x,_) (y,_) ->  compare x y) ys in
  Array.map (fun (p,i) -> (i,p)) ys

let sort_saving_permutation ?(compare=Pervasives.compare) (xs:'a array) : (int array) * ('a array) =
  let ys = Array.mapi (fun i x -> (x,i)) xs in
  let () = Array.sort (fun (x,_) (y,_) ->  compare x y) ys in
  let xs, js = split ys in
  (js, xs)

(** {b Example}:
{[ let xs = [| 23; 21; 10; 5; 9; 0; 2; 12; |] ;;
let js, ys = ArrayExtra.sort_saving_permutation xs ;;
val js : int array = [|5; 6; 3; 4; 2; 7; 1; 0|]
val ys : int array = [|0; 2; 5; 9; 10; 12; 21; 23|]
ys = (ArrayExtra.apply_permutation js xs) ;;
 : bool = true 
xs = (ArrayExtra.undo_permutation js ys) ;;
 : bool = true ]} *)
let apply_permutation js xs = 
  let ys = Array.copy xs in
  let () = Array.iteri (fun i j -> ys.(i) <- xs.(j)) js in
  ys

let undo_permutation js xs = 
  let ys = Array.copy xs in
  let () = Array.iteri (fun i j -> ys.(j) <- xs.(i)) js in
  ys
  
(** {b Example}:
{[# int_seq 3 10 2 ;;
  : int array = [|3; 5; 7; 9|]
]}*)
let int_seq ~min ~max ~incr =
 let rec loop x =
  if x>max then [] else x::(loop (x+incr))
 in
 let xs = loop min in
 Array.of_list xs

let float_seq ~min ~max ~incr =
 let tollerance = incr /. 2. in
 let max = max +. tollerance in
 let rec loop x =
  if x>max then [] else x::(loop (x+.incr))
 in
 let xs = loop min in
 Array.of_list xs

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

let shared_property f s =
 let l = Array.length s in
 if l=0 then true else
 let y = lazy (f s.(0)) in
 let p = (fun x -> (f x)=(Lazy.force y)) in
 let rec loop i =
  if i>=l then true else
  (p s.(i)) && loop (i+1)
 in loop 1

let dichotomic_search ?(a=0) ?b v x =
 let rec loop a b =
   if a=b then a else
   let i = (a+b)/2 in
   if x > v.(i) then loop (i+1) b else
   if (i>0) && (v.(i-1) >= x) then loop a (i-1) else
   i
 in
 let b = match b with None -> (Array.length v)-1 | Some b -> b in
 let i = loop a b in
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

(** {b Example}:
{[
# let a = Array.init 10 (fun i->i*10);;
val a : int array = [|0; 10; 20; 30; 40; 50; 60; 70; 80; 90|]
# dichotomic_index_of_first_element_gt (-1) a ;;
  : int option = Some 0
# dichotomic_index_of_first_element_gt 0 a ;;
  : int option = Some 1
# dichotomic_index_of_first_element_gt 15 a ;;
  : int option = Some 2
# dichotomic_index_of_first_element_gt 20 a ;;
  : int option = Some 3
# dichotomic_index_of_first_element_gt 80 a ;;
  : int option = Some 9
# dichotomic_index_of_first_element_gt 85 a ;;
  : int option = Some 9
# dichotomic_index_of_first_element_gt 90 a ;;
  : int option = None
]}
*)
let dichotomic_index_of_first_element_gt ?a ?b x v =
 let l = Array.length v in
 let last_index = l-1 in
 match dichotomic_search ?a ?b v x with
 | true , i ->
    if (i = last_index) then None else Some (i+1)
 | false, i when (i = last_index) ->
     if v.(i) > x then Some i else None
 | false, i -> Some i

(**  {b Example}:
{[
# let a = Array.init 10 (fun i->i*10);;
val a : int array = [|0; 10; 20; 30; 40; 50; 60; 70; 80; 90|]
# dichotomic_index_of_last_element_lt (-1) a ;;
  : int option = None
# dichotomic_index_of_last_element_lt 0 a ;;
  : int option = None
# dichotomic_index_of_last_element_lt 5 a ;;
  : int option = Some 0
# dichotomic_index_of_last_element_lt 10 a ;;
  : int option = Some 0
# dichotomic_index_of_last_element_lt 15 a ;;
  : int option = Some 1
# dichotomic_index_of_last_element_lt 85 a ;;
  : int option = Some 8
# dichotomic_index_of_last_element_lt 90 a ;;
  : int option = Some 8
# dichotomic_index_of_last_element_lt 95 a ;;
 : int option = Some 9
]}
*)
let dichotomic_index_of_last_element_lt ?a ?b x v =
 let l = Array.length v in
 let last_index = l-1 in
 match dichotomic_search ?a ?b v x with
 | true , i ->
    if (i = 0) then None else Some (i-1)
 | false, i when (i = last_index) ->
     if v.(i) < x then Some i else
     if i>0 && v.(i-1) < x then Some (i-1) else
     None
 | false, i -> Some (i-1)


let for_all2 f xs ys = for_all (fun i x -> f i x ys.(i)) xs
let exists2  f xs ys = exists  (fun i x -> f i x ys.(i)) xs

let iter2  f a b = Array.iteri (fun i a -> f a b.(i)) a
let iteri2 f a b = Array.iteri (fun i a -> f i a b.(i)) a

let map2  f a b = Array.mapi (fun i a -> f a b.(i)) a
let mapi2 f a b = Array.mapi (fun i a -> f i a b.(i)) a

(** {b Example}:
{[ map_fold (fun s i x -> (x+s,s+x)) 0 [|0;1;2;3;4;5|] ;;
  : int array = [|0; 1; 3; 6; 10; 15|]
]} *)
let map_fold f s0 xs =
  let n = Array.length xs in
  if n = 0 then [||] else begin
    let (y0, s1) = f s0 0 (xs.(0)) in
    let result = Array.create n y0 in
    let state = ref s1 in
    for i = 1 to n-1 do
      let (y,z) = f (!state) i (xs.(i)) in
      result.(i) <- y ;
      state := z;
    done;
    result
  end

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

let fold_binop f s =
 let l = Array.length s in
 if l = 0 then invalid_arg "ArrayExtra.fold_binop: I cannot fold an empty array" else
 let rec loop acc i =
  if i>=l then acc else
  let acc = f acc s.(i) in
  loop acc (i+1)
 in loop s.(0) 1

(** Similar to [List.partition] but for arrays and with many classes.
{b Example}:
{[
# partition (fun x -> x mod 3) [|0;1;2;3;4;5;6;7;8;9|] ;;
  : int array array = [|[|0; 3; 6; 9|]; [|1; 4; 7|]; [|2; 5; 8|]|]
]} *)
let partition =
  let errmsg = "ArrayExtra.partition: classifier must provide only non-negative integers" in
  fun ?(min_size=0) f a ->
  (* f' is a dynamically type checking version of f: *)
  let f' x = (let y = f x in (if (y<0) then invalid_arg errmsg); y) in
  let max_index = Array.fold_left (fun s x -> max s (f' x)) (-1) a in
  if max_index = -1 then Array.create min_size [||] else
  let ls = Array.create (max min_size (max_index+1)) [] in
  (Array.iter (fun x -> let c = f x in ls.(c) <- x :: ls.(c)) a);
  let result = Array.map (fun l -> Array.of_list (List.rev l)) ls in
  result

let partitioni =
  let errmsg = "ArrayExtra.partitioni: classifier must provide only non-negative integers" in
  fun ?(min_size=0) f a ->
  (* f' is a dynamically type checking version of f: *)
  let f' i x = (let y = f i x in (if (y<0) then invalid_arg errmsg); y) in
  let max_index = fold_lefti (fun i s x -> max s (f' i x)) (-1) a in
  if max_index = -1 then Array.create min_size [||] else
  let ls = Array.create (max min_size (max_index+1)) [] in
  (Array.iteri (fun i x -> let c = f i x in ls.(c) <- x :: ls.(c)) a);
  let result = Array.map (fun l -> Array.of_list (List.rev l)) ls in
  result
  
let amass ~size = 
  if size <= 0 then invalid_arg "ArrayExtra.amass: size must be greater than zero" else
  partitioni (fun i x -> i/size)
 
(** {b Example}:
{[
# cut [1;2;3;0;2] [|0;1;2;3;4;5;6;7;8;9|];;
  : int array list = [[|0|]; [|1; 2|]; [|3; 4; 5|]; [||]; [|6; 7|]]
]} *)
let cut ~lengths xs =
  let start_len_list_of_lengths xs =
    let js,_ = List.fold_left (fun (js,n) x -> ((n+x)::js,n+x)) ([0],0) xs in
    List.combine (List.rev (List.tl js)) xs
  in
  let start_len_list = start_len_list_of_lengths lengths in
  try
    List.map (fun (start, len) -> Array.sub xs start len) start_len_list
  with Invalid_argument "Array.sub" -> invalid_arg "ArrayExtra.cut"


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

(** By default the best is the minimal element, i.e. the choice function is set by default to [min]. *)
let best ?(choice=min) = function
 | [||] -> invalid_arg "ArrayExtra.best: empty array"
 | xs   -> fold_lefti (fun i (j,x) y -> if (choice x y) <> x then (i,y) else (j,x)) (0, xs.(0)) xs

let max ?(gt=(>)) xs =
 fold_lefti (fun i (j,x) y -> if gt y x then (i,y) else (j,x)) (0, xs.(0)) xs

let min ?(gt=(>)) xs =
 fold_lefti (fun i (j,x) y -> if gt y x then (j,x) else (i,y)) (0, xs.(0)) xs

(** Example (from the module [Ipv6]):
{[let search_longest_sequence_of_zeros ?leftmost =
  ArrayExtra.search_longest_sequence ?leftmost ((=)0);; ]}*)
let search_longest_sequence ?leftmost p x =
  let (last,_, acc) =
    fold_lefti
       (fun i ((n,j), inseq, acc) v ->
           if p v then
             (* if we are in a sequence, increment the counter: *)
             (if inseq then ((n+1,j), true, acc)
                       (* else reset the counter registering the current result in the list: *)
                       else ((1,i), true, ((n,j)::acc)))
           else
             (* register that the sequence is finished inseq=false: *)
             ((n,j), false, acc))
       ((0,0), false, []) x
  in
  let njs = last::acc in
  let (best_n, best_j) =
    let candidates = match leftmost with
    | None -> njs
    | Some () -> List.rev njs
    in
    List.fold_left
      (fun ((best_n, best_j) as a) ((n,j) as b) -> if n>best_n then b else a)
      (List.hd candidates)
      (List.tl candidates)
  in
  if best_n > 0 then Some (best_j, best_n) else None
;;


let random_permutation xs =
  let n = Array.length xs in
  let js = Array.init n (fun i -> i) in
  let () = 
    Array.iteri 
      (fun i x -> 
        let choice = i + (Random.int (n-i)) in
        js.(i)<-js.(choice); js.(choice)<-x)
      js
  in
  Array.map (fun j -> xs.(j)) js

let frequence pred xs =
 let n = Array.length xs in
 if n=0 then invalid_arg "ArrayExtra.frequence: empty array" else (* continue: *)
 let occ = Array.fold_left (fun occ x -> if (pred x) then occ+1 else occ) 0 xs in
 let freq = (float_of_int occ) /. (float_of_int n) in
 (occ, freq)

let count pred xs = 
 let n = Array.length xs in
 if n=0 then invalid_arg "ArrayExtra.count: empty array" else (* continue: *)
 Array.fold_left (fun occ x -> if (pred x) then occ+1 else occ) 0 xs
 