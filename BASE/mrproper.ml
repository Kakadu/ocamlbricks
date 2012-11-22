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

type t = (one_shot * Thunk.t) Stack.t
and one_shot = bool

let dress_thunk ?unprotected ?one_shot thunk =
  let thunk =
    match unprotected with
    | Some () -> thunk
    | None    -> Thunk.protect thunk
  in
  let result =
    match one_shot with
    | Some () -> (true, Thunk.linearize thunk) (* not really needed *)
    | None    -> (false, thunk)
  in
  result

let stack_rev t0 = 
  let t1 = Stack.create () in
  Stack.iter (fun x -> Stack.push x t1) t0;
  t1
  
let stack_remove_one_shot_thunks t0 = 
  let t1 = Stack.create () in
  Stack.iter (fun (b,x) -> if b then () else Stack.push (b,x) t1) t0;
  Stack.clear t0;
  Stack.iter (fun x -> Stack.push x t0) t1;
  ()

let register_thunk ?unprotected ?one_shot thunk t =
  let thunk = dress_thunk ?unprotected ?one_shot thunk in 
  Stack.push (thunk) t

let register_lazy ?unprotected lazy_action t =
 let thunk = Thunk.of_lazy lazy_action in
 register_thunk ?unprotected ~one_shot:() thunk t

let reverse_according_to ?fifo t = 
  match fifo with
  | None    -> t
  | Some () -> stack_rev t
 
let force ?fifo t = 
  Stack.iter (fun (_,f) -> f ()) (reverse_according_to ?fifo t);
  stack_remove_one_shot_thunks t;
  ()

module Make_instance (Unit:sig end) = struct

(* The global stack of thunks:  *)
let global_t = Stack.create ()

let fifo_discipline = ref None (* false *)

let set_fifo_discipline () = 
  fifo_discipline := Some () (* true *)
  
let force () = force ?fifo:!fifo_discipline global_t

let register_thunk ?unprotected ?one_shot action =
  register_thunk ?unprotected ?one_shot action global_t

let register_lazy ?unprotected action =
  register_lazy ?unprotected action global_t
  
let exit code =
  (force ());
  (Pervasives.exit code)

end (* functor Make_instance *)
  
class obj ?fifo () = 
  let (register_thunk, register_lazy, force) =
    let module M = Make_instance(struct end) in
    let () = Option.iter M.set_fifo_discipline fifo in
    (M.register_thunk, M.register_lazy, M.force)
  in
  object (self)
    method register_thunk = register_thunk
    method register_lazy  = register_lazy
    method force = force
end

(* Make and include the global instance: *)
include Make_instance (struct end)
