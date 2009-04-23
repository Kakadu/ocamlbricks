(** Additional features for the standard library [Unix]. *)

type filename   = string
type foldername = string
type content    = string
val current_umask : int
val touch : ?perm:Unix.file_perm -> filename -> unit

(** {2 Copying files} *)

val file_copy   : ?buffer_size:int -> ?perm:Unix.file_perm -> filename -> filename -> unit
val file_append : ?buffer_size:int -> ?perm:Unix.file_perm -> filename -> filename -> unit

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

val kill_safe : int -> int -> unit

exception Signal_forward of int
exception Waitpid

val create_process_and_wait :
  ?stdin:Endpoint.Source.t ->
  ?stdout:Endpoint.Sink.t  ->
  ?stderr:Endpoint.Sink.t  ->
  ?pseudo:string option -> ?forward:int list -> string -> string list -> int

val run   : ?shell:command -> ?trace:bool -> ?input:string -> command -> string * Unix.process_status
val shell : ?shell:command -> ?trace:bool -> ?input:string -> command -> string

(** {b Asynchronous version} *)

type future = (int * string * string) Future.t

val future :
  ?stdin:Endpoint.Source.t ->
  ?stdout:Endpoint.Sink.t  ->
  ?stderr:Endpoint.Sink.t  ->
  ?pseudo:program -> ?forward:int list -> program -> program list -> future