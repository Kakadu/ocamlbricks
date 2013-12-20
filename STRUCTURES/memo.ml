(* This file is part of our reusable OCaml BRICKS library
   Copyright (C) 2007-2011 Jean-Vincent Loddo

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

module Log = Ocamlbricks_log

(** The default size of for hash tables. *)
let default_size = 251;;

(** Memoize a function. *)
let memoize_and_get_table ?trace_faults ?(size=default_size) f =
  let ht = Hashtbl.create size in
  let f' =
    match trace_faults with
    | None ->
	(function x ->
	  try
	    Hashtbl.find ht x
	  with Not_found ->
	    begin
	      let y = f x in
	      let () = Hashtbl.add ht x y in
	      y
	    end)
    | Some () ->
	(function x ->
	  try
	    Hashtbl.find ht x
	  with Not_found ->
	    begin
	      Log.printf1
		"Memo.memoize: cache fault for input with hash key %d: function must be called.\n"
		(Hashtbl.hash x) ;
	      let y = f x in
	      let () = Hashtbl.add ht x y in
	      y
	    end)
    in
    (f', ht)

let memoize ?trace_faults ?size f = fst (memoize_and_get_table ?trace_faults ?size f)

