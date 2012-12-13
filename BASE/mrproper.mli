(* This file is part of our reusable OCaml BRICKS library
   Copyright (C) 2009 2012  Jean-Vincent Loddo

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

(** Register actions to perform exiting the program or destroying a
    temporary structure.

On the one hand, a definition as the following could be written at the beginning of a program:
{[ let exit = Mrproper.exit ;; ]}
and could be used for exiting the program.
On the other hand, anywhere in the program you could register some actions in order to leave
the program cleanly, as for instance:
{[ let filename = temp_file ~perm:0o755 ~suffix:".sh" () in
 let () = Mrproper.register_lazy (lazy (Unix.unlink filename)) in
 ...]}

Note that actions are internally registered in a {b stack} and are thus performed in the reversed order
with respect to the registration (insertion). You can switch to a fifo discipline with
the function [set_fifo_discipline].
*)

(** Push a thunk into the global stack of thunks. *)
val register_thunk : ?unprotected:unit -> ?one_shot:unit -> Thunk.t -> unit

(** Push a lazy action into the global stack of thunks.
    Lazy actions are forced to be one-shot: when unprotected, the
    thunk will not re-raise the same exception forcing the thunk execution twice. *)
val register_lazy  : ?unprotected:unit -> unit Lazy.t -> unit

(** Exit the program performing all registered actions in the global stack.*)
val exit  : int -> 'a

(** Perform the list (stack) of registered actions. *)
val force : unit -> unit

(** Switch to a fifo discipline (instead of lifo as by default). *)
val set_fifo_discipline : unit -> unit

(** Make a {e local} object-oriented mrproper structure.
    A typical use is to connect the method [force] to the destruction of
    a temporary structure, as for instance a widget.

    {b Example}:
{[  let window = GWindow.window () in
  let mrproper = new Mrproper.obj () in
  ..
  mrproper#register_lazy (lazy ...);
  mrproper#register_lazy (lazy ...);
  ..
  let _ = window#connect#destroy ~callback:mrproper#force in
  ..
]}*)
class obj : ?fifo:unit -> unit -> object
  method register_thunk : ?unprotected:unit -> ?one_shot:unit -> Thunk.t -> unit
  method register_lazy  : ?unprotected:unit -> unit Lazy.t -> unit
  method force : unit -> unit
end
