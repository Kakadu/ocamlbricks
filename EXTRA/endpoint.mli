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

module Source :
 sig
  type t =
  | Unix_descr of Unix.file_descr
  | In_channel of in_channel
  | Filename   of string
  | String     of string
  | Empty

  val to_file_descr : t -> Unix.file_descr * bool
  val to_string : t -> string
 end

module Sink :
 sig
  type t =
  | Unix_descr  of Unix.file_descr
  | Out_channel of out_channel
  | Filename    of string
  | Fun_thread  of (Unix.file_descr -> unit)
  | String_queue of String_queue.t
  | Trash

  val to_file_descr : t -> Unix.file_descr * bool
  val to_string : t -> string
 end
