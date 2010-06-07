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
