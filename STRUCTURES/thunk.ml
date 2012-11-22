(* This file is part of ocamlbricks
   Copyright (C) 2012  Jean-Vincent Loddo

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
   
type t = unit -> unit

(** Transform a thunk in a one-shot thunk, i.e. a thunk that cannot be executed more than once (the second call and subsequent calls return immediately with the token [()]) *)
let linearize thunk =
 let already_called = ref false in
 fun () ->
   if !already_called then () else
     begin
       already_called := true;
       thunk ()
     end

(** The resulting thunk will be protected from exceptions. 
    It will translate all exceptions in the output [()]. *)     
let protect thunk =
 (fun x -> try thunk x with _ -> ())

(** Simply the application. *)
let apply thunk = thunk ()

(** Conversion from lazy. Note that the result is directly a linear (one-shot) thunk because of the lazyness. 
    However, the tool [linearize] still remains interesting for this kind of thunks. 
    Actually, if the lazy value raises an exception, the resulting thunk raises this exception for each call, 
    while the linearized one raises this exception only once. *)
let of_lazy l = fun () -> Lazy.force l
