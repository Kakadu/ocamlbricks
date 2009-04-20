(* This file is part of our reusable OCaml BRICKS library
   Copyright (C) 2007  Jean-Vincent Loddo

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

(* Do not remove the following line: it's an ocamldoc workaround!*)
(** *)

(** Extra definitions. *)
module Extra = struct

(** A {e filename} is a string. *)
type filename = string;;

(** A {e foldername} is a string. *)
type foldername = string;;

(** A {e content} is a string. *)
type content = string;;

(** The current value of umask. *)
let current_umask = 
 let old = Unix.umask 0 in 
 let   _ = Unix.umask old in 
 old
;;

(** Create a file if necessary with the given permissions 
   (by default equal to [0o640]). *)
let touch ?(perm=0o640) (fname:filename) : unit = 
  let fd = (Unix.openfile fname [Unix.O_CREAT] perm) in (Unix.close fd)
;;

(* {2 Copying} *)

(** Support for copying files. 
-- From {{:http://www.enseignement.polytechnique.fr/profs/informatique/Didier.Remy/system/camlunix/fich.html}Xavier Leroy and Didier Remy's OS course, Chapter 2}. *)
module Copylib = struct

    open Unix;;
    
    let buffer_size = 8192;;
    let buffer = String.create buffer_size;;
    
    let file_copy ?(perm=0o666) ?(flag=O_TRUNC) input_name output_name =
      let fd_in = openfile input_name [O_RDONLY] 0 in
      let fd_out = openfile output_name [O_WRONLY; O_CREAT; flag] perm in
      let rec copy_loop () =
        match read fd_in buffer 0 buffer_size with
          0 -> ()
        | r -> ignore (write fd_out buffer 0 r); copy_loop () in
      copy_loop ();
      close fd_in;
      close fd_out;;

end;; (* module Copylib *)

(** Copy a file into another. Optional permissions (by default [0o640]) concern of course the target. *)
let file_copy ?(perm=0o640) (x:filename) (y:filename) = Unix.handle_unix_error (Copylib.file_copy ~perm x) y ;;

(** Append a file into another. Optional permissions (by default [0o640]) concern of course the target. *)
let file_append ?(perm=0o640) (x:filename) (y:filename) = Unix.handle_unix_error (Copylib.file_copy ~perm ~flag:Unix.O_APPEND x) y ;;

(* {2 Saving strings} *)

(** Write or rewrite the file with the given content. 
    If the file does not exists, it is created with the given permission 
   (set by default to [0o640]). *)
let put ?(perm=0o640) (fname:filename) (x:content) : unit = 
  let fd = (Unix.openfile fname [Unix.O_CREAT; Unix.O_WRONLY; Unix.O_TRUNC] perm) in 
  let n = String.length x in
  ignore (Unix.write fd x 0 n);
  (Unix.close fd)
;;

(** Alias for [put]. *)
let rewrite = put;;

(** Similar to the function [put] described above, but the content is {b appended} instead of rewrited.
    If the file doesn't exists, it is created with the given permissions (set by default to [0o640]). *)
let append ?(perm=0o640) (fname:filename) (x:content) = 
  let fd = (Unix.openfile fname [Unix.O_CREAT; Unix.O_WRONLY; Unix.O_APPEND] perm) in 
  let n  = String.length x in
  ignore (Unix.write fd x 0 n);
  (Unix.close fd)
;;

(* {2 Loading strings} *)

(** Return the {b whole} content (caution!) of the file 
    as a string. Use only for small files. 
    Great for making pipelines. For instance, 
    the following pipeline catches the first line of [/etc/fstab] containing 
    the substring "hda1": 
{[# "/etc/fstab" => ( cat || String.to_list || Str.grep ".*hda1.*" || hd ) ]}*)
let rec cat (fname:filename) = 
  let fd = (Unix.openfile fname [Unix.O_RDONLY] 0o640) in 
  let len = 16*1024 in
  let buff = String.create len in
  let rec boucle () = 
    begin
    let n = (Unix.read fd buff 0 len) in
    let s = String.sub buff 0 n in
    if (n<len) then s
    else s^(boucle ())
    end in
  boucle ()
;;


(* {2 Temporary files} *)

(** Support for this section. *)
module Templib = struct

(** General function for creating temporary files or directories in a parent directory
    with some permissions and with a prefix and suffix for the name. 
    The function returns the name of the created file or directory.
*)
let rec temp_name ~(dir:bool) ~(perm:Unix.file_perm) ~(parent:string) ~(prefix:string) ~(suffix:string) () = 
  begin
   let rnd = Random.int (1024*1024*1023) in
   let candidate = (Filename.concat parent (prefix^(string_of_int rnd)^suffix)) in
   if (Sys.file_exists candidate) then (temp_name ~dir ~perm  ~parent ~prefix ~suffix ())
   else 
     begin
         if dir then (Unix.mkdir candidate perm) 
         else (touch candidate ~perm) ;
         candidate
     end
  end
;;
end;; (* module Templib *)

(** Create a temporary directory in a parent directory. By default:
   - permissions [perm] are set to [0o755]
   - the parent directory is set to ["/tmp"]
   - the prefix and suffix of the name are the empty string

The function returns the name of the created directory. 

{b Example}:
{[# temp_dir ();;
  : string = "/tmp/819618234"

# temp_dir ~suffix:".txt" ();;
  : string = "/tmp/625724514.txt"
]}*)
let rec temp_dir ?(perm=0o755) ?(parent="/tmp") ?(prefix="") ?(suffix="") () =
  Templib.temp_name ~dir:true ~parent ~perm ~prefix ~suffix ()
;;

(** Create a temporary file in a parent directory. The optional parameters have the same meaning
    of the function [temp_dir]. In addition, the parameter [content] may be set in order
    to put it in the created file. By default the [content] is the empty string. 
    The function returns the name of the created directory. 
*)
let rec temp_file ?(perm=0o644) ?(parent="/tmp") ?(prefix="") ?(suffix="") ?(content:string="") () =
  let fname = (Templib.temp_name ~dir:false ~perm ~parent ~prefix ~suffix ()) in
  (if content<>"" then (rewrite fname content));
  fname
;;

(** More secure functions using the [TMPDIR] environment variable and implemented as wrappers of [Filename.open_temp]. *)
module TMPDIR = struct

let default_prefix = (Filename.basename Sys.executable_name)^".";;

let rec open_temp 
  ?(perm=0o640) 
  ?(prefix=default_prefix) 
  ?(suffix="") () =
  (try
    let (filename,ch) = Filename.open_temp_file prefix suffix in
    let fd = Unix.descr_of_out_channel ch in
    (Unix.chmod filename perm);
    (filename,fd)
   with e ->
     (Printf.eprintf "%s: cannot create a temporary file; set the environment variable TMPDIR to resolve this problem.\n" Sys.executable_name);
     (flush stderr);
     raise e)

let temp_file
 ?(perm=0o640) 
 ?(prefix=default_prefix) 
 ?(suffix="") () =
 let (filename,fd) = open_temp ~perm ~prefix ~suffix () in
 (Unix.close fd);
 filename

end;;

(* {2 Kind} *)

(** Heuristic that tries to convert a char into a value of the type: 

    [Unix.file_kind = S_REG | S_DIR | S_CHR | S_BLK | S_LNK | S_FIFO | S_SOCK] 

    The input character must belong to the set ['f';'d';'c';'b';'h';'p';'s'], 
    following the convention of the standard unix [test] command.
    Otherwise, the result is [None].*)
let file_kind_of_char = function
 | 'f'       -> Some Unix.S_REG  (*  Regular file  *)
 | 'd'       -> Some Unix.S_DIR  (*  Directory  *)
 | 'c'       -> Some Unix.S_CHR  (*  Character device  *)
 | 'b'       -> Some Unix.S_BLK  (*  Block device  *)
 | 'h' | 'L' -> Some Unix.S_LNK  (*  Symbolic link  *)
 | 'p'       -> Some Unix.S_FIFO (*  Named pipe  *)
 | 'S'       -> Some Unix.S_SOCK (*  Socket  *) 
 |  _        -> None
;;

(* {2 Directories} *)

(** [iter_dir f dirname] iterate the function [f] on each entry of the directory [dirname].
-- From {{:http://www.enseignement.polytechnique.fr/profs/informatique/Didier.Remy/system/camlunix/fich.html}Xavier Leroy and Didier Remy's OS course, Chapter 2}. *)
let iter_dir f dirname =
      let d = Unix.opendir dirname in
      try while true do f (Unix.readdir d) done
      with End_of_file -> Unix.closedir d
;;


(* {3 Find} *)

(** Support for finding in a directory hierarchy. 
-- From {{:http://www.enseignement.polytechnique.fr/profs/informatique/Didier.Remy/system/camlunix/fich.html}Xavier Leroy and Didier Remy's OS course, Chapter 2}. *)
module Findlib = struct

    exception Hidden of exn

    let hide_exn f x = try f x with exn -> raise (Hidden exn);;
    let reveal_exn f x = try f x with Hidden exn -> raise exn;;

    open Unix;;

    let find on_error on_path follow depth roots =
      let rec find_rec depth visiting filename =
        try
          let infos = (if follow then stat else lstat) filename in
          let continue = hide_exn (on_path filename) infos in
          let id = infos.st_dev, infos.st_ino in
          if infos.st_kind = S_DIR && depth > 0 && continue &&
            (not follow || not (List.mem id visiting))
          then
            let process_child child =
              if (child <> Filename.current_dir_name &&
                  child <> Filename.parent_dir_name) then
                let child_name = Filename.concat filename child in
                let visiting =
                  if follow then id :: visiting else visiting in
                 find_rec (depth-1) visiting child_name in
                 iter_dir process_child filename
        with Unix_error (e, b, c) -> hide_exn on_error (e, b, c) in
      reveal_exn (List.iter (find_rec depth [])) roots
;;

end;; (* module Findlib *)


(** Find something in an input directory. This function is an interface for the
    tool [Findlib.find]. 

    The default assignements are: 
    - [follow=false]
    - [maxdepth=1024]
    - [kind='_'] which implies no condition on kind 
    - [name=""]  which implies no condition on name. 
    
    The set of characters corresponding
    to the kind of file are the same of the standard [test] unix command (i.e. 
    ['f';'d';'c';'b';'h';'p';'s']); see the function {!file_kind_of_char} 
    for more details.  

    {b Warning:} use this function with caution: the good version
    of this function will be a version returning a sequence (stream) instead of
    a list. 

{b Examples}:
{[# find "/etc/ssh/" ;;
  : string list = ["/etc/ssh/"; "/etc/ssh/ssh_config"; "/etc/ssh/sshd_config";
 "/etc/ssh/ssh_host_key"; "/etc/ssh/ssh_host_dsa_key.pub";
 "/etc/ssh/ssh_host_rsa_key.pub"; "/etc/ssh/moduli";
 "/etc/ssh/ssh_host_key.pub"; "/etc/ssh/ssh_host_dsa_key";
 "/etc/ssh/ssh_host_rsa_key"]

# find ~kind:'d' "/etc/ssh/" ;;
  : string list = ["/etc/ssh/"]

# find ~name:"moduli" "/etc/ssh/" ;;
  : string list = ["/etc/ssh/moduli"]
]} *)
let find ?(follow=false) ?(maxdepth=1024) ?(kind='_') ?(name="") (root:string) : string list =
      let result = ref [] in

	
      let action = match (file_kind_of_char kind, name) with
       | (None    , ""   ) -> fun p infos -> result := (p::!result)
       | ((Some k), ""   ) -> fun p infos -> if (infos.Unix.st_kind = k)    then result := (p::!result);
       | (None    , n    ) -> fun p infos -> if ((Filename.basename p) = n) then result := (p::!result)
       | ((Some k), n    ) -> fun p infos -> if (infos.Unix.st_kind = k) && ((Filename.basename p) = n) 
					       then result := (p::!result); in

      let action p infos = (action p infos; true) in

      let on_error (e, b, c) = prerr_endline (c ^ ": " ^ Unix.error_message e) in
        Unix.handle_unix_error (Findlib.find on_error action follow maxdepth) [root];
        List.rev (!result)
;;



(* {2 Password} *)

(** Support for input passwords. 
-- From {{:http://www.enseignement.polytechnique.fr/profs/informatique/Didier.Remy/system/camlunix/fich.html}Xavier Leroy and Didier Remy's OS course, Chapter 2}. *)
module Passwdlib = struct

     open Unix;;

     let read_passwd message =
      match
        try
          let default = tcgetattr stdin in
          let silent =
            { default with
              c_echo = false;
              c_echoe = false;
              c_echok = false;
              c_echonl = false;
            } in
          Some (default, silent)
        with _ -> None
      with
      | None -> input_line Pervasives.stdin
      | Some (default, silent) ->
          print_string message;
          flush Pervasives.stdout;
          tcsetattr stdin TCSANOW silent;
          try
            let s = input_line Pervasives.stdin in
            tcsetattr stdin TCSANOW default; s
          with x ->
            tcsetattr stdin TCSANOW default; raise x;;

end;; (* Passwdlib *)

(** Prompt for a password. The terminal is set for hiding the characters read from keyboard. *)
let read_passwd prompt = Passwdlib.read_passwd prompt;;


(* {2 Process status printers} *)

(** Process status printers; {b examples}:
{[# Process_status.printf "The result is '%s'\n" (snd (run "unexisting-program"));;
The result is 'Unix.WEXITED 127'
  : unit = ()

# Process_status.printf "The result is '%s'\n" (snd (run "ls"));;
The result is 'Unix.WEXITED 0'
 : unit = () ]} *)
module Process_status = PreludeExtra.Extra.Printers0 (struct
    type t = Unix.process_status
    let string_of = function
    | Unix.WEXITED   code   -> (Printf.sprintf "Unix.WEXITED %d" code)
    | Unix.WSIGNALED signal -> (Printf.sprintf "Unix.WSIGNALED %d" signal)
    | Unix.WSTOPPED  signal -> (Printf.sprintf "Unix.WSTOPPED %d" signal)
    end);;

(* {2 Running} *)

(** A {e command} is something understandable by the shell. *)
type command = string;;

(** A {e program} is a file binary (which will be found by the system in [PATH]). *)
type program = string;;

open Endpoint;;

(** [kill_safe pid signal] send the [signal] to the process [pid] ignoring exceptions. *)
let kill_safe pid signal =
  try Unix.kill pid signal with _ -> ()
;;

exception Signal_forward of int;;
exception Waitpid;;

type waiting_events = {
  mutable forwarded_signal : int option ;
  mutable process_status   : Unix.process_status option;
  mutable waitpid_exn      : bool ;
 };;

let new_waiting_events () = {
  forwarded_signal = None  ;
  process_status   = None  ;
  waitpid_exn      = false ;
 };;


let rec wait_child child_pid events =
 try begin
  let (_, process_status) = (Unix.waitpid [] child_pid) in
  (events.process_status <- Some process_status);
  match process_status with 
  | Unix.WEXITED   code   -> () (* return *)
  | Unix.WSIGNALED signal 
  | Unix.WSTOPPED  signal -> (events.forwarded_signal <- Some signal); wait_child child_pid events
 end with 
 | Unix.Unix_error(_,_, _) -> (events.waitpid_exn <- true)
 ;;

 let new_handler child_pid events = 
  Sys.Signal_handle 
   (fun s -> (events.forwarded_signal <- Some s); 
             (kill_safe child_pid s); 
             (wait_child child_pid events))
 ;;

 (** Create process with [?stdin=Unix.stdin], [?stdout=Unix.stdout] and [?stderr=Unix.stderr] connected
     to a given source and sinks, then wait until its termination.
     During waiting, some signals could be forwarded by the father to the child specifying the argument [?(forward = [Sys.sigint; Sys.sigabrt; Sys.sigquit; Sys.sigterm; Sys.sigcont])].
     The two last parameters are the program (binary) and its list of actual parameters. The process is created with the primitive [Unix.create_process].
     If the process exits with [Unix.WEXITED code] the code is returned. Otherwise an exception is raised, more specifically:
     - [Signal_forward s] is raised if the father has transmitted a signal (certainly the reason of the violent termination of the child);
     - [Waitpid] is raised if the internal call to [Unix.waitpid] has failed for some unknown reasons.*)
 let create_process_and_wait 
 ?(stdin  = Source.Unix_descr Unix.stdin) 
 ?(stdout = Sink.Unix_descr   Unix.stdout) 
 ?(stderr = Sink.Unix_descr   Unix.stderr)
 ?(pseudo = None) 
 ?(forward = [Sys.sigint; Sys.sigabrt; Sys.sigquit; Sys.sigterm; Sys.sigcont])
 program arguments =

 let (stdin,  stdin_must_be_closed )  = Source.to_file_descr stdin in
 let (stdout, stdout_must_be_closed)  = Sink.to_file_descr stdout    in
 let (stderr, stderr_must_be_closed)  = Sink.to_file_descr stderr    in

 let events = new_waiting_events () in
 let name = match pseudo with None -> program | Some name -> name in
 let argv = (Array.of_list (name :: arguments)) in
 let child_pid = (Unix.create_process program argv stdin stdout stderr) in
 let handler = new_handler child_pid events in
 let handler_backups = List.map  (fun s -> (s, (Sys.signal s handler))) forward in
 let restore_handlers () = List.iter (fun (s,h) -> Sys.set_signal s h) handler_backups in
 (wait_child child_pid events);
(restore_handlers ());

 (if  stdin_must_be_closed then Unix.close stdin); 
 (if stdout_must_be_closed then Unix.close stdout); 
 (if stderr_must_be_closed then Unix.close stderr); 

 match events with
  | { process_status   = Some (Unix.WEXITED code); forwarded_signal = None;  waitpid_exn = false } -> code
  | { process_status   = Some (Unix.WEXITED code); forwarded_signal = Some s } when s=Sys.sigcont  -> code
  | { forwarded_signal = Some s ; waitpid_exn = true } -> (raise (Signal_forward s))
  | { waitpid_exn      = true  }                       -> (raise Waitpid)
  | _ -> (assert false)
 ;;

(* Convert a string option into a shell specification. The shell "bash" is our default. *)
let shell_of_string_option = function
| None       -> "bash"
| Some shell -> shell
;;

(** [run command] exec the shell ([bash] by default) with arguments [\["-c";command\]] and return the pair (output, exit-code).
    A string can be specified as standard input for the command. The flag [trace] (by default set to [false])
    permits to obtain some informations about the running on [stderr]. {b Examples}:
{[# run "ls /etc/*tab";;
  : string * Unix.process_status =
("/etc/crontab\n/etc/fstab\n/etc/inittab\n/etc/mtab\n/etc/quotatab\n", Unix.WEXITED 0)

# run ~input:"hello" "cat";;
  : string * Unix.process_status = ("hello", Unix.WEXITED 0)

# run ~shell:"dash" ~input:"HELLO" "head -n 1 /etc/passwd /dev/stdin | cut -c-15";;
  : string * Unix.process_status =
("==> /etc/passwd\nat:x:25:25:Batc\n\n==> /dev/stdin \nHELLO\n", Unix.WEXITED 0)
]} *)
let run ?shell ?(trace:bool=false) ?input (cmd:command) : string * Unix.process_status =
  let shell = shell_of_string_option shell in
  let (stdin,inp) = match input with
   | None   -> (Source.Unix_descr Unix.stdin, "<stdin>")
   | Some x -> (Source.String x, x)
  in
  let queue  = String_queue.create () in
  let stdout = Sink.String_queue queue in
  let code = create_process_and_wait ~stdin ~stdout shell ["-c";cmd] in
  let out  = (String_queue.concat queue) in
  (if trace then begin
      Printf.eprintf "UnixExtra.run: tracing: input   is '%s'\n" inp;
      Printf.eprintf "UnixExtra.run: tracing: command is '%s'\n" cmd;
      Printf.eprintf "UnixExtra.run: tracing: output  is '%s'\n" out;
      flush stderr;
      end);
  (out, Unix.WEXITED code)
;;

(** As [run], but ignoring the exit-code. This function is
    simply a shortcut for the composition of [run] with [fst]. {b Examples}:
 
{[# shell "date";;
  : string = "ven avr 13 18:34:02 CEST 2007\n"

# String.Text.Matrix.of_string (shell "wc -l /etc/*tab");;
  : string list list =
[["8"; "/etc/crontab"]; ["20"; "/etc/fstab"]; ["98"; "/etc/inittab"];
 ["11"; "/etc/mtab"]; ["127"; "/etc/naccttab"]; ["9"; "/etc/quotatab"];
 ["273"; "total"]]
]}*)
let shell ?shell ?(trace:bool=false) ?(input:string="") cmd =
  fst(run ~shell:(shell_of_string_option shell) ~trace ~input cmd) 
;;

(** A Unix future is a future containing the exit code and the two strings outcoming from stdout and stderr. *)
type future = (int * string * string) Future.t ;;

let future ?stdin ?stdout ?stderr ?pseudo ?(forward=[]) (program:program) argv_list : future =
 begin
 let stdin  = match stdin  with None -> Source.Empty | Some x -> x
 in
 let define_sink_and_string_maker optional_sink =
   (match optional_sink with
    | Some x -> (x, fun () -> "")
    | None   ->
        let q = String_queue.create () in
        (Sink.String_queue q), (fun () -> String_queue.concat q)
    )
 in
 let (stdout, stdout_string_maker) = define_sink_and_string_maker stdout in
 let (stderr, stderr_string_maker) = define_sink_and_string_maker stderr in
 let future = Future.future (fun () ->
     begin
      let code = create_process_and_wait ~stdin ~stdout ~stderr ~pseudo ~forward program argv_list in
      let stdout_string = stdout_string_maker () in
      let stderr_string = stderr_string_maker () in
      (code, stdout_string, stderr_string)
     end) () in
 future
 end
;;

end;; (* module Extra *)

(** Redefinition of module [Unix]. *)
module Unix = struct
  include Unix;;
  include Extra;;
end;;
