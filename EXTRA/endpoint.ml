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

(** Abstract channel endpoints (sources and sinks). *)

(** Abstract source (or negative) channel endpoints. *)
module Source = struct

(** The abstract type of a source endpoint *)
type t =
  | Unix_descr of Unix.file_descr  (** An already opened unix descriptor. *)
  | In_channel of in_channel       (** An already opened pervasives input channel. *)
  | Filename   of string           (** A file name. *)
  | String     of string           (** A string content. *)
  | Empty                          (** A shorthand for [String ""] *)

(** Source to string conversion. *)
let to_string = function
 | Unix_descr  c when c=Unix.stdin -> "stdin"
 | Unix_descr  _ -> "Unix descriptor"
 | In_channel  c when c=stdin -> "stdin"
 | In_channel  c -> "in_channel"
 | Filename    f -> f
 | String      s -> "String \""^s^"\""
 | Empty         -> "Empty"

(** Create a unix file descriptor from a source if necessary.
    The function returns also a flag indicating if the descriptor must be closed.
    If the user has given directly a descriptor (unix or standard), the descriptor
    do not must be closed. If the user has given a filename, the on-the-fly created
    descriptor must be closed. *)
let to_file_descr =
 let in_descr_of_string s =
  let len = (String.length s) in
  let (pread,pwrite) = Unix.pipe () in
  let count = (Unix.write pwrite s 0 len) in
  (assert (count = len));
  (Unix.close pwrite);
   pread
 in function
 | Unix_descr  d -> (d, false)
 | In_channel  c -> ((Unix.descr_of_in_channel c), false)
 | Filename    s -> ((Unix.openfile s [Unix.O_RDONLY] 0o640), true)
 | String      s -> ((in_descr_of_string s),true)
 | Empty         -> ((in_descr_of_string ""),true)

end


(** Abstract sink (or positive) channel endpoints. *)
module Sink = struct

(** The abstract type of a sink endpoint. *)
type t =
  | Unix_descr   of Unix.file_descr            (** An already opened unix descriptor. *)
  | Out_channel  of out_channel                (** An already opened pervasives output channel. *)
  | Filename     of string                     (** A file name, (re)writing. *)
  | Filename_append of string                  (** A file name, appending. *)
  | Fun_thread   of (Unix.file_descr -> unit)  (** A consumer function. *)
  | String_queue of String_queue.t             (** A string queue. *)
  | Trash                                      (** A sort of /dev/null. *)

(** Sink to string conversion. *)
let to_string = function
 | Unix_descr  c when c=Unix.stdout -> "stdout"
 | Unix_descr  c when c=Unix.stderr -> "stderr"
 | Unix_descr  _ -> "Unix descriptor"
 | Out_channel c when c=stdout -> "stdout"
 | Out_channel c when c=stderr -> "stderr"
 | Out_channel c -> "out_channel"
 | Filename    f -> f
 | Filename_append f -> ">>"^f
 | Fun_thread   _ -> "Fun_thread"
 | String_queue _ -> "String_queue"
 | Trash          -> "Trash"

(** Create a unix file descriptor from a sink if necessary.
    The function returns also a flag indicating if the descriptor must be closed.
    If the user has given directly a descriptor (unix or standard), the descriptor
    do not must be closed. If the user has given a filename, a treatment function or
    a string queue, the on-the-fly created descriptor must be closed. *)
let to_file_descr =
 let out_descr_of_fun_thread f =
  let (pread,pwrite) = Unix.pipe () in
  let try_close d = try (Unix.close d) with _ -> () in
  let wrap f d =
    (let res = try (f d) with e -> ((try_close d); raise e) in (try_close d); res) in
  (ignore (Thread.create (wrap f) pread));
   pwrite
 in function
 | Unix_descr   d -> (d, false)
 | Out_channel  c -> ((Unix.descr_of_out_channel c), false)
 | Filename        s -> ((Unix.openfile s [Unix.O_WRONLY; Unix.O_CREAT] 0o640), true)
 | Filename_append s -> ((Unix.openfile s [Unix.O_APPEND; Unix.O_CREAT] 0o640), true)
 | Fun_thread   f -> ((out_descr_of_fun_thread f),true)
 | String_queue q ->
    let f = fun fd -> String_queue.append_from_descr ~release:true q fd; in
    ((out_descr_of_fun_thread f),true)
 | Trash ->
    let block_size = 1024 in
    let buff = String.create block_size in
    let rec trash_loop fd =
      let n = (Unix.read fd buff 0 block_size) in
      if (n=0) then () else trash_loop fd
    in
    ((out_descr_of_fun_thread trash_loop),true)

end
