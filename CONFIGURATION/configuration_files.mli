(* This file is part of our reusable OCaml BRICKS library
   Copyright (C) 2008 Luca Saiu

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

(** Simple implementation of application-wise configuration files, implemented as shell scripts.
    Configuration files are looked for (by default) in "standard" places like /etc,
    the user's home directory and the shell environment.
    There is a priority:
    - System-wise files in /etc
    - User's files in ~
    - The shell environment at application startup time.
*)

class configuration :
  ?software_name:string ->
  ?file_names:string list ->
  variables:string list ->
  ?read_environment:bool ->
  unit ->
  object
    method bool   : string -> bool
    method float  : string -> float
    method int    : string -> int
    method list   : string -> string list
    method string : string -> string
  end
