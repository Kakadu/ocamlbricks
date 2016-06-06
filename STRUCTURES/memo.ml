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
let memoize_and_get_table ?trace_faults ?trace_success ?(size=default_size) f =
  let ht = Hashtbl.create size in
  let calls, success = (ref 0), (ref 0) in
  let f' =
    match trace_faults, trace_success with
    | None, None ->
	(function x ->
	  try
	    Hashtbl.find ht x
	  with Not_found ->
	    begin
	      let y = f x in
	      let () = Hashtbl.add ht x y in
	      y
	    end)
    | (Some ()), None ->
	(function x ->
	  let () = incr calls in
	  try
            let result = Hashtbl.find ht x in
            let () = incr success in
	    result
	  with Not_found ->
	    begin
	      Log.printf2
		"Memo.memoize: cache fault for hash key %d (cumulated faults %4.1f%%).\n"
		(Hashtbl.hash x) (PervasivesExtra.percentage_fraction ~decimals:1 (!calls - !success) !calls);
	      let y = f x in
	      let () = Hashtbl.add ht x y in
	      y
	    end)
    | None, (Some ()) ->
        (function x ->
          let () = incr calls in
          try
            let result = Hashtbl.find ht x in
            let () = incr success in
            let () = Log.printf2 "Memo.memoize: success for hash key %d (cumulated success %4.1f%%).\n" 
               (Hashtbl.hash x) (PervasivesExtra.percentage_fraction ~decimals:1 !success !calls) 
            in
            result
          with Not_found ->
            begin
              let y = f x in
              let () = Hashtbl.add ht x y in
              y
            end)
    | (Some ()), (Some ()) ->
        (function x ->
          let () = incr calls in
          try
            let result = Hashtbl.find ht x in
            let () = incr success in
            let () = Log.printf2 "Memo.memoize: success for hash key %d (cumulated success %4.1f%%).\n" 
               (Hashtbl.hash x) (PervasivesExtra.percentage_fraction ~decimals:1 !success !calls) 
            in
            result
          with Not_found ->
            begin
              Log.printf2
                "Memo.memoize: cache fault for hash key %d (cumulated faults %4.1f%%).\n"
                (Hashtbl.hash x) (PervasivesExtra.percentage_fraction ~decimals:1 (!calls - !success) !calls);
              let y = f x in
              let () = Hashtbl.add ht x y in
              y
            end)
    in
    (f', ht)

let memoize ?trace_faults ?trace_success ?size f = fst (memoize_and_get_table ?trace_faults ?trace_success ?size f)

