(* This file is part of our reusable OCaml BRICKS library
   Copyright (C) 2010  Jean-Vincent Loddo

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

(** Make extra definitions for Hashtbl. *)

type ('a, 'b) t = ('a, 'b) Hashtbl.t

let search t k  = try Some (Hashtbl.find t k) with Not_found -> None

let to_assoc_list t = Hashtbl.fold (fun x y l -> (x,y)::l) t []

let remove_all t x =
 let ys = Hashtbl.find_all t x in
 List.iter (fun _ -> Hashtbl.remove t x) ys

module Make (H : Hashtbl.HashedType) = struct
 include Hashtbl.Make(H)

 let search t k  = try Some (find t k) with Not_found -> None
 let to_assoc_list t = fold (fun x y l -> (x,y)::l) t []

 let remove_all t x =
  let ys = find_all t x in
  List.iter (fun _ -> remove t x) ys

end
 