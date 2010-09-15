(* This file is part of our reusable OCaml BRICKS library
   Copyright (C) 2009 Jean-Vincent Loddo

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

(** Additional features for the standard library [Unix]. *)

type filename   = string
type foldername = string
type content    = string

val apply_ignoring_Unix_error : ('a -> unit) -> 'a -> unit

(** {2 File permissions} *)

type symbolic_mode = (bool*bool*bool)*(bool*bool*bool)*(bool*bool*bool)

val update_symbolic_mode :
  ?u:unit -> ?g:unit -> ?o:unit -> ?a:unit -> ?r:bool -> ?w:bool -> ?x:bool ->
  symbolic_mode -> symbolic_mode

val get_umask : unit -> symbolic_mode
val set_umask : (bool*bool*bool) -> (bool*bool*bool) -> (bool*bool*bool) -> unit
val update_umask :
  ?u:unit -> ?g:unit -> ?o:unit -> ?a:unit -> ?r:bool -> ?w:bool -> ?x:bool ->
  unit -> unit

val test_access : ?r:unit -> ?w:unit -> ?x:unit -> filename -> bool
val touch : ?perm:Unix.file_perm -> filename -> unit

val get_perm : filename -> symbolic_mode
val set_perm :
  ?u:unit -> ?g:unit -> ?o:unit -> ?a:unit -> ?r:bool -> ?w:bool -> ?x:bool ->
  filename -> unit

(** {2 Copying files} *)

val file_copy   : ?buffer_size:int -> ?perm:Unix.file_perm -> filename -> filename -> unit
val file_append : ?buffer_size:int -> ?perm:Unix.file_perm -> filename -> filename -> unit
val file_move   : filename -> filename -> unit

(** {2 Saving strings} *)

val put     : ?perm:Unix.file_perm -> filename -> content -> unit
val rewrite : ?perm:Unix.file_perm -> filename -> content -> unit
val append  : ?perm:Unix.file_perm -> filename -> content -> unit

(** {2 Loading strings} *)

val cat : filename -> string

(** {2 Temporary files} *)

val temp_dir :
  ?perm:Unix.file_perm ->
  ?parent:string -> ?prefix:string -> ?suffix:string -> unit -> string

val temp_file :
  ?perm:Unix.file_perm ->
  ?parent:string ->
  ?prefix:string -> ?suffix:string -> ?content:content -> unit -> string

module TMPDIR :
  sig
    val default_prefix : string
    val open_temp :
      ?perm:Unix.file_perm ->
      ?prefix:string -> ?suffix:string -> unit -> string * Unix.file_descr
    val temp_file :
      ?perm:Unix.file_perm ->
      ?prefix:string -> ?suffix:string -> unit -> string
  end


(** {2 File kind} *)

val file_kind_of_char : char -> Unix.file_kind option

(** {2 Directories} *)

val iter_dir : (string -> 'a) -> string -> unit
val find : ?follow:bool -> ?maxdepth:int -> ?kind:char -> ?name:string -> string -> string list

(** {2 Password} *)

val read_passwd : string -> string

(** {2 Process status printers} *)

module Process_status :
  sig
    type t = Unix.process_status
    val string_of     : t -> string
    val print         : Unix.process_status -> unit
    val prerr         : Unix.process_status -> unit
    val print_endline : Unix.process_status -> unit
    val prerr_endline : Unix.process_status -> unit
    val fprintf : out_channel -> (string -> 'a, out_channel, unit) format -> Unix.process_status -> 'a
    val eprintf :                (string -> 'a, out_channel, unit) format -> Unix.process_status -> 'a
    val printf  :                (string -> 'a, out_channel, unit) format -> Unix.process_status -> 'a
    val sprintf :                (string -> 'a, unit, string) format      -> Unix.process_status -> 'a
  end

(** {2 Managing external programs} *)

type command = string
type program = string

(* val is_executable_in_PATH : program -> bool *)
val is_executable_in_PATH : program -> string option

val system_or_fail : ?hide_output:bool -> ?hide_errors:bool -> command -> unit

val kill_safe : int -> int -> unit

exception Signal_forward of int
exception Waitpid

val create_process_and_wait :
  ?stdin:Endpoint.Source.t ->
  ?stdout:Endpoint.Sink.t  ->
  ?stderr:Endpoint.Sink.t  ->
  ?pseudo:string ->
  ?forward:int list ->
  ?register_pid:(int->unit) ->
  program -> string list -> int

type process_result = int * string * string

val create_process_and_wait_then_get_result :
  ?stdin:Endpoint.Source.t ->
  ?stdout:Endpoint.Sink.t  ->
  ?stderr:Endpoint.Sink.t  ->
  ?pseudo:string ->
  ?forward:int list ->
  ?register_pid:(int->unit) ->
  program -> string list -> process_result

val run   : ?shell:command -> ?trace:bool -> ?input:string -> command -> string * Unix.process_status
val shell : ?shell:command -> ?trace:bool -> ?input:string -> command -> string

(** {b Asynchronous version} *)

val future :
  ?stdin:Endpoint.Source.t ->
  ?stdout:Endpoint.Sink.t  ->
  ?stderr:Endpoint.Sink.t  ->
  ?pseudo:string ->
  ?forward:int list ->
  ?register_pid:(int->unit) ->
  program -> string list -> process_result Future.t

val kfuture :
  ?stdin:Endpoint.Source.t ->
  ?stdout:Endpoint.Sink.t  ->
  ?stderr:Endpoint.Sink.t  ->
  ?pseudo:string ->
  ?forward:int list ->
  ?register_pid:(int->unit) ->
  program -> string list -> (int -> string -> string ->'a) -> 'a Future.t

val script :
  ?stdin:Endpoint.Source.t ->
  ?stdout:Endpoint.Sink.t  ->
  ?stderr:Endpoint.Sink.t  ->
  ?pseudo:string ->
  ?forward:int list ->
  ?register_pid:(int->unit) ->
  content -> string list -> process_result

val is_process_alive : int -> bool

module Process : sig

 type process_status =
 | WUNCHANGED
 | WEXITED of int
 | WSIGNALED of int
 | WSTOPPED of int
 | WCONTINUED

 type wait_flag =
 | WNOHANG
 | WUNTRACED
 | WCONTINUE

 val waitpid : wait_flag list -> int -> int * process_status

 type t = process_status
 val string_of     : t -> string
 val print         : t -> unit
 val prerr         : t -> unit
 val print_endline : t -> unit
 val prerr_endline : t -> unit
 val fprintf : out_channel -> (string -> 'a, out_channel, unit) format -> t -> 'a
 val eprintf :                (string -> 'a, out_channel, unit) format -> t -> 'a
 val printf  :                (string -> 'a, out_channel, unit) format -> t -> 'a
 val sprintf :                (string -> 'a, unit, string) format      -> t -> 'a

end

val date : ?dash:string -> ?dot:string -> ?colon:string -> ?no_time:unit -> ?no_date:unit
  -> unit -> string

val resolve_symlink : string -> string
val is_symlink : string -> bool
