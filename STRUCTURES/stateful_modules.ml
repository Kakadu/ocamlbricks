(* This file is part of ocamlbricks
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


module type Type = sig type t val name:string option end

module Variable (Type:Type) = struct

  let failmsg = match Type.name with
  | None   -> Printf.sprintf "Stateful_modules.Variable.get: content is None"
  | Some x -> Printf.sprintf "Stateful_modules.Variable.get: content of %s is None" x
  
  type t = Type.t
  let content = ref None
  let set (x:t) = (content := Some x)
  let unset () = (content := None)
  let get () = !content
  let extract () = match !content with
   | Some x -> x
   | None   -> failwith failmsg 
end

module Thread_shared_variable (Type:Type) = struct
  include Variable (Type)
  module Mutex = MutexExtra.Recursive
  let mutex = Mutex.create ()
  (* we provide these new methods: *)
  let apply_with_mutex f x = Mutex.apply_with_mutex mutex f x
  let lock () = Mutex.lock mutex
  let unlock () = Mutex.unlock mutex
  (* and the thread-safe versions of accessors: *)
  let set x = apply_with_mutex set x
  let unset () = apply_with_mutex unset ()
  let get x = apply_with_mutex get x
  let extract x = apply_with_mutex extract x
end

