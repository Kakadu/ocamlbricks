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

(** Register actions to perform exiting the program.

On the one hand, a definition as the following could be written at the beginning of a program:
{[ let exit = Mrproper.exit ~ignore_errors:true ;; ]}
and could be used for exiting the program.
On the other hand, anywhere in the program you could register some actions in order to leave
the program cleanly, as for instance:
{[ let filename = temp_file ~perm:0o755 ~suffix:".sh" () in
 let () = Mrproper.register (lazy (Unix.unlink filename)) in
 ...]}

Note that actions are internally registered in a {b stack} and are thus performed in the reversed order
with respect to the registration (insertion).
*)

(** An action is a deferred value of type [unit]. *)
type action = unit lazy_t

(* A global stack with thunks that must be executed exiting. *)
let mrproper = Stack.create ()

(** Force Mrproper to perform the list (stack) of registered actions. *)
let work ?(ignore_errors=false) () =
  let action =
    (match ignore_errors with
    | true  -> (fun x -> try Lazy.force x with _ -> ())
    | false -> Lazy.force
    ) in
  (Stack.iter action mrproper);
  (Stack.clear mrproper)

(** Register an action,i.e. push it into the internal stack. *)
let register ?(ignore_errors=false) deferred =
  let deferred =
    (match ignore_errors with
    | true  -> lazy (try Lazy.force deferred with _ -> ())
    | false -> deferred
    ) in
   (Stack.push deferred mrproper)

(** Exit the program performing all registered actions in the stack.*)
let exit ?(ignore_errors=false) code =
  (work ~ignore_errors ());
  (Pervasives.exit code)

(** Make a {e local} mrproper structure via a functorial interface.
    A typical use is to connect the function [work] to the destruction of
    a temporary structure, as for instance a widget. The function [work]
    of a local mrproper is registered into the global mrproper setting
    the optional parameter [work_at_exit] to [true].
    The value [ignore_errors] defines the default of this parameter for
    all generated functions.

    {b Example}:
{[  let window = GWindow.window () in
  let module Mrproper = Mrproper.Make (struct let ignore_errors=true let work_at_exit = false end) in
  ..
  Mrproper.register (lazy ...);
  Mrproper.register (lazy ...);
  ..
  let _ = window#connect#destroy ~callback:Mrproper.work in
  ..
]}*)
module Make
 (Defaults: sig
    val ignore_errors : bool
    val work_at_exit : bool
  end) = struct

 let global_register = register

 let mrproper = Stack.create ()

 let work ?(ignore_errors=Defaults.ignore_errors) () =
  let action =
    (match ignore_errors with
    | true  -> (fun x -> try Lazy.force x with _ -> ())
    | false -> Lazy.force
    ) in
  (Stack.iter action mrproper);
  (Stack.clear mrproper)

 let side_effect = match Defaults.work_at_exit with
  | true  -> global_register (lazy (work ()))
  | false -> ()

 let register ?(ignore_errors=Defaults.ignore_errors) deferred =
  let deferred =
    (match ignore_errors with
    | true  -> lazy (try Lazy.force deferred with _ -> ())
    | false -> deferred
    ) in
   (Stack.push deferred mrproper)

end
