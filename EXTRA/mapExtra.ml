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

module Extend = functor (M:Map.S) -> struct
  include M

  (** Extra functions: *)

  let copy (m : 'a t) : 'a t = fold add m empty

  let filter (p : key -> 'a -> bool) (m : 'a t) : 'a t =
    fold (fun k a m' -> if p k a then add k a m' else m') m empty

  let of_list ?(acc=empty) (xs : (key * 'a) list) : 'a t =
    List.fold_left (fun m (k,a) -> add k a m) acc xs

  let to_list ?(acc=[]) ?(reverse=false) (m : 'a t) =
    let acc = if reverse then (List.rev acc) else acc in
    let l = fold (fun k a xs -> (k,a)::xs) m acc in
    if reverse then List.rev l else l

  let domain ?(reverse=true) (m : 'a t) =
    fst (List.split (to_list ~reverse m))

  let codomain ?(reverse=true) (m : 'a t) =
    snd (List.split (to_list ~reverse m))

  let restrict m ks =
    List.fold_left (fun m' k -> try add k (find k m) m' with Not_found -> m') empty ks

  let substract m ks =
    List.fold_left (fun m' k -> remove k m') m ks
 end


module Make (Ord : Map.OrderedType) = Extend (Map.Make (Ord))

module String_map = Make (struct type t = string let compare = Pervasives.compare end)
module Int_map    = Make (struct type t = int    let compare = Pervasives.compare end)

