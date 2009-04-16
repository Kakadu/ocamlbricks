(* This file is part of our reusable OCaml BRICKS library
   Copyright (C) 2009  Jean-Vincent Loddo

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

(* Authors:
 * - Luca Saiu: function with_mutex from recursive_mutex.ml
 * - Jean-Vincent Loddo: functorization
 *)


(** Additional features for the standard module [Mutex].

{b Usage}:
-    {[ open MutexExtra;; ]}
-    {[ module Mutex = MutexExtra.Mutex;; ]}
The previous phrases are equivalent and allow you to access to additional features for Mutex.

You can give a look to the {!MutexExtra.Extend} functor for more informations about these features.
*)

(** Extra definitions for Mutex. *)
module Extend
 (Mutex : sig
  type t 
  val lock     : t -> unit
  val unlock   : t -> unit
 end) = struct

(** Execute thunk in a synchronized block, and return the value returned
    by the thunk. If executing thunk raises an exception the same exception
    is propagated, after correctly unlocking the mutex. *)
 let with_mutex mutex thunk =
  Mutex.lock mutex;
  try
    let result = thunk () in
    Mutex.unlock mutex;
    result
  with e -> begin
    Mutex.unlock mutex;
    (Printf.eprintf
      "MutexExtra.Make.with_mutex: exception %s raised in critical section. Unlocking and re-raising.\n"
      (Printexc.to_string e));
    raise e;
  end

end

(** Redefinition of the standard [Mutex]. *)
module Mutex = struct
 include Mutex
 include Extend (Mutex)
end
