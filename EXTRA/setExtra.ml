(* This file is part of our reusable OCaml BRICKS library
   Copyright (C) 2010 Jean-Vincent Loddo

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

module Extend = functor (S:Set.S) -> struct
  include S

  let copy (s : t) : t = fold add s empty

  let of_list ?(acc=empty) (xs : elt list) : t =
    List.fold_left (fun m e -> add e m) acc xs

  let to_list ?(acc=[]) ?(reverse=false) (m : t) =
    let acc = if reverse then (List.rev acc) else acc in
    let l = fold (fun e xs -> e::xs) m acc in
    if reverse then List.rev l else l

  let uniq xs = to_list (of_list xs)

 end

module Make = functor (Ord:Set.OrderedType) -> Extend (Set.Make (Ord))

module String_set = Extend (Set.Make (struct type t = string let compare = Pervasives.compare end))
module Int_set    = Extend (Set.Make (struct type t = int    let compare = Pervasives.compare end))

module Destructive = struct

 module Make (Ord : Set.OrderedType) = struct
  module Persistent = Make (Ord)
  type elt = Ord.t
  type t = Persistent.t ref
  let  create () = ref Persistent.empty
  let is_empty t = Persistent.is_empty (!t)
  let mem x t = Persistent.mem x (!t)
  let add x t = (t := Persistent.add x !t)
  let singleton x = ref (Persistent.singleton x)
  let remove x t = (t := Persistent.remove x !t)
  let union t0 t1 = (t0 := Persistent.union (!t0) (!t1))
  let inter t0 t1 = (t0 := Persistent.inter (!t0) (!t1))
  let diff  t0 t1 = (t0 := Persistent.diff  (!t0) (!t1))
  let compare t0 t1 = Persistent.compare (!t0) (!t1)
  let equal   t0 t1 = Persistent.equal   (!t0) (!t1)
  let subset  t0 t1 = Persistent.subset  (!t0) (!t1)
  let iter f t = Persistent.iter f (!t)
  let fold f t = Persistent.fold f (!t)
  let for_all f t = Persistent.for_all f (!t)
  let exists f t = Persistent.exists f (!t)
  let filter f t = (t := Persistent.filter f (!t))

  let partition f t =
    let (s0,s1) = Persistent.partition f (!t) in
    (ref s0, ref s1)

  let cardinal t = Persistent.cardinal (!t)
  let elements t = Persistent.elements (!t)
  let min_elt t = Persistent.min_elt (!t)
  let max_elt t = Persistent.max_elt (!t)
  let choose t = Persistent.choose (!t)

  let split x t =
   let (s0,b,s1) = Persistent.split x (!t) in
   (ref s0, b, ref s1)

  let copy t = ref (!t)

  let of_list ?acc l =
    let acc = match acc with None -> None | Some t -> Some (!t) in
    ref (Persistent.of_list ?acc l)

  let to_list ?acc ?reverse t  = Persistent.to_list ?acc ?reverse (!t)

end


end (* Persistent *)