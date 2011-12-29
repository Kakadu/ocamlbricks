(* This file is part of our reusable OCaml BRICKS library
   Copyright (C) 2007  Jean-Vincent Loddo

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


(*module Fold :
  sig
    val commacat         : string list -> string
    val semicolon        : string list -> string
    val nospacecommacat  : string list -> string
    val nospacesemicolon : string list -> string
    val dotcat           : string list -> string
    val newlinecat       : string list -> string
    val blankcat         : string list -> string
    val slashcat         : string list -> string
  end*)


(*(** Examples of applications of [big] constructor
    in conjonction with the [merge] function. *)
module Fold = struct

 (** Merge a string list with the separator [" , "]. *)
 let commacat = big (merge " , ");;

 (** Merge a string list with the separator ["; "]. *)
 let semicolon = big (merge "; ");;

 (** Merge a string list with the separator [","]. *)
 let nospacecommacat = big (merge ",");;

 (** Merge a string list with the separator [";"]. *)
 let nospacesemicolon = big (merge ";");;

 (** Merge a string list with the separator ["."]. *)
 let dotcat = big (merge ".");;

 (** Merge a string list with the separator ["\n"]. *)
 let newlinecat = big (merge "\n");;

 (** Merge a string list with the separator [" "]. *)
 let blankcat = big (merge " ");;

 (** Merge a string list with the separator ["/"]. *)
 let slashcat = big (merge "/");;

end (* module Fold *)*)


let try_finalize f x finally y =
      let res = try f x with exn -> finally y; raise exn in
      finally y;
      res
;;




(** Additionnal tools for list manipulations *)
module LList = struct


 (** {2 combine 2-8} *)

 let combine2 = List.combine ;;

 let rec combine5 l1 l2 l3 l4 l5 = match (l1,l2,l3,l4,l5) with
  | []    , []    , []    , []    , []     -> []
  | x1::r1, x2::r2, x3::r3, x4::r4, x5::r5 -> (x1,x2,x3,x4,x5)::(combine5 r1 r2 r3 r4 r5)
  | _ -> raise (Invalid_argument "combine5")
 ;;

 let rec combine6 l1 l2 l3 l4 l5 l6 = match (l1,l2,l3,l4,l5,l6) with
  | []    , []    , []    , []    , []     , []     -> []
  | x1::r1, x2::r2, x3::r3, x4::r4, x5::r5 , x6::r6 -> (x1,x2,x3,x4,x5,x6)::(combine6 r1 r2 r3 r4 r5 r6)
  | _ -> raise (Invalid_argument "combine6")
 ;;

 let rec combine7 l1 l2 l3 l4 l5 l6 l7 = match (l1,l2,l3,l4,l5,l6,l7) with
  | []    , []    , []    , []    , []     , []     , []     -> []
  | x1::r1, x2::r2, x3::r3, x4::r4, x5::r5 , x6::r6 , x7::r7 -> (x1,x2,x3,x4,x5,x6,x7)::(combine7 r1 r2 r3 r4 r5 r6 r7)
  | _ -> raise (Invalid_argument "combine7")
 ;;

 let rec combine8 l1 l2 l3 l4 l5 l6 l7 l8 = match (l1,l2,l3,l4,l5,l6,l7,l8) with
  | []    , []    , []    , []    , []     , []     , []     , []     -> []
  | x1::r1, x2::r2, x3::r3, x4::r4, x5::r5 , x6::r6 , x7::r7 , x8::r8 -> (x1,x2,x3,x4,x5,x6,x7,x8)::(combine8 r1 r2 r3 r4 r5 r6 r7 r8)
  | _ -> raise (Invalid_argument "combine8")
 ;;

 (** {2 split 2-8} *)

 let split2 = List.split ;;

 let rec split3 l = match l with
 | [] -> ([],[],[])
 | (x1,x2,x3)::r -> let (s1,s2,s3) = (split3 r) in (x1::s1,x2::s2,x3::s3)
 ;;

 let rec split4 l = match l with
 | [] -> ([],[],[],[])
 | (x1,x2,x3,x4)::r -> let (s1,s2,s3,s4) = (split4 r) in (x1::s1,x2::s2,x3::s3,x4::s4)
 ;;

 let rec split5 l = match l with
 | [] -> ([],[],[],[],[])
 | (x1,x2,x3,x4,x5)::r -> let (s1,s2,s3,s4,s5) = (split5 r) in (x1::s1,x2::s2,x3::s3,x4::s4,x5::s5)
 ;;

 let rec split6 l = match l with
 | [] -> ([],[],[],[],[],[])
 | (x1,x2,x3,x4,x5,x6)::r -> let (s1,s2,s3,s4,s5,s6) = (split6 r) in (x1::s1,x2::s2,x3::s3,x4::s4,x5::s5,x6::s6)
 ;;

 let rec split7 l = match l with
 | [] -> ([],[],[],[],[],[],[])
 | (x1,x2,x3,x4,x5,x6,x7)::r -> let (s1,s2,s3,s4,s5,s6,s7) = (split7 r) in (x1::s1,x2::s2,x3::s3,x4::s4,x5::s5,x6::s6,x7::s7)
 ;;

 let rec split8 l = match l with
 | [] -> ([],[],[],[],[],[],[],[])
 | (x1,x2,x3,x4,x5,x6,x7,x8)::r -> let (s1,s2,s3,s4,s5,s6,s7,s8) = (split8 r) in (x1::s1,x2::s2,x3::s3,x4::s4,x5::s5,x6::s6,x7::s7,x8::s8)
 ;;

end ;;

let g x = let _ = Thread.delay (float_of_int x) in ignore (Sys.command ("touch /tmp/pluto."^(string_of_int x))) ;;
let xs = Array.init 31 (fun i -> 34+i);;  
let tks = Array.map (fun x -> ThreadExtra.create_killable g x) xs ;;
Array.iteri (fun i (t,k) -> Thread.delay 0.1; if i mod 3 = 0 then () else k ()) tks ;;
let (t1,k1) = ThreadExtra.create_killable g 96;;
let (t2,k2) = ThreadExtra.create_killable g 97;;
let (t3,k3) = ThreadExtra.create_killable g 98;;
let (t4,k4) = ThreadExtra.create_killable g 99;;

