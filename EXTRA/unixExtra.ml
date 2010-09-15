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

(** A {e filename} is a string. *)
type filename = string;;

(** A {e foldername} is a string. *)
type foldername = string;;

(** A {e content} is a string. *)
type content = string;;

let apply_ignoring_Unix_error f x =
 try f x with Unix.Unix_error (_,_, _) -> ()
;;

(** The {e user}, {e group} and {e other} permissions [(r,w,x),(r,w,x),(r,w,x)]. *)
type symbolic_mode = (bool*bool*bool)*(bool*bool*bool)*(bool*bool*bool)

let list_of_symbolic_mode = function
  ((u_r, u_w, u_x), (g_r, g_w, g_x), (o_r, o_w, o_x)) -> [u_r; u_w; u_x ; g_r; g_w; g_x; o_r; o_w; o_x ]

let symbolic_mode_of_list = function
  | [u_r; u_w; u_x ; g_r; g_w; g_x; o_r; o_w; o_x ] ->  ((u_r, u_w, u_x), (g_r, g_w, g_x), (o_r, o_w, o_x))
  | xs -> invalid_arg (Printf.sprintf
                        "symbolic_mode_of_list: the length of list is %d but it expected to be 9"
                        (List.length xs))

(** Update a symbolic mode using optial parameters with a meaning similar to the command
    line options of the unix utility [chmod]. *)
let update_symbolic_mode ?u ?g ?o ?a ?r ?w ?x ((u_r, u_w, u_x), (g_r, g_w, g_x), (o_r, o_w, o_x)) =
 let extract = function Some x -> x | None -> assert false in
 let user_involved = (u<>None || a<>None) in
 let u_r = if user_involved && r<>None then (extract r) else u_r in
 let u_w = if user_involved && w<>None then (extract w) else u_w in
 let u_x = if user_involved && x<>None then (extract x) else u_x in
 let group_involved = (g<>None || a<>None) in
 let g_r = if group_involved && r<>None then (extract r) else g_r in
 let g_w = if group_involved && w<>None then (extract w) else g_w in
 let g_x = if group_involved && x<>None then (extract x) else g_x in
 let other_involved = (o<>None || a<>None) in
 let o_r = if other_involved && r<>None then (extract r) else o_r in
 let o_w = if other_involved && w<>None then (extract w) else o_w in
 let o_x = if other_involved && x<>None then (extract x) else o_x in
 ((u_r, u_w, u_x), (g_r, g_w, g_x), (o_r, o_w, o_x))

(** The current value of umask. *)
let get_umask () : symbolic_mode =
 let current = Unix.umask 0 in
 let   _     = Unix.umask current in
 symbolic_mode_of_list (List.map not (Bit.bits_as_booleans_of_int ~length:9 current))
;;

(** Set umask using (a currified version of) a symbolic mode. *)
let set_umask um gm om =
 let i = Bit.int_of_bits_as_booleans (List.map not (list_of_symbolic_mode (um,gm,om))) in
 ignore (Unix.umask i)
;;

(** Update the default file creation mask specifying who is updated: user ([?u]) and/or group ([?g])
    and/or other ([?o]) and/or all ([?a]), and what you whant to update and how ([true/false]):
    read ([?r]) and/or write ([?w]) and/or execution ([?x]). *)
let update_umask ?u ?g ?o ?a ?r ?w ?x () =
 let sm = get_umask () in
 let (um,gm,om) = update_symbolic_mode ?u ?g ?o ?a ?r ?w ?x sm in
 set_umask um gm om
;;

(** Get the permissions of a file: a triple of booleans respectively for user, group and other. *)
let get_perm (fname:filename) : symbolic_mode =
 try
  let i = (Unix.stat fname).Unix.st_perm in
  symbolic_mode_of_list (Bit.bits_as_booleans_of_int ~length:9 i)
 with
  | Unix.Unix_error (Unix.ENOENT,"stat", _) -> failwith ("UnixExtra.get_perm: cant stat the file "^fname)
;;

(** Set the permissions of a file specifying who is involved: user ([?u]) and/or group ([?g])
    and/or other ([?o]) and/or all ([?a]), and what you whant to set and how ([true/false]):
    read ([?r]) and/or write ([?w]) and/or execution ([?x]). *)
let set_perm ?u ?g ?o ?a ?r ?w ?x (fname:filename) =
 let sm = get_perm fname in
 let sm = update_symbolic_mode ?u ?g ?o ?a ?r ?w ?x sm in
 let file_perm = Bit.int_of_bits_as_booleans (list_of_symbolic_mode sm) in
 Unix.chmod fname file_perm
 ;;

(** Could the process perform some operations on the file: read ([?r]) and/or write ([?w]) and/or
    execution ([?x])?*)
let test_access ?r ?w ?x filename : bool =
 let xs = [(r,Unix.R_OK); (w,Unix.W_OK); (x,Unix.X_OK)] in
 let xs = List.filter (fun (v,_)-> v<>None) xs in
 let xs = Unix.F_OK::(List.map snd xs) in
 try
   let _ = Unix.access filename xs in true
 with Unix.Unix_error (_,_,_) -> false
;;

(** Create a file if necessary with the given permissions
   (by default equal to [0o644]). *)
let touch ?(perm=0o644) (fname:filename) : unit =
 try (* file exists *)
  let stat = (Unix.stat fname) in
  let size = stat.Unix.st_size in
  let fd = Unix.openfile fname [Unix.O_WRONLY] 0o644 in
  Unix.ftruncate fd size;
  Unix.close fd;
 with
  | Unix.Unix_error (Unix.EACCES,"open", _) -> failwith ("UnixExtra.touch: cannot open file "^fname)
  | Unix.Unix_error (Unix.ENOENT,"stat", _) ->
  begin (* file doesn't exist *)
   let fd = (Unix.openfile fname [Unix.O_CREAT] perm) in
   (Unix.close fd)
  end
;;

(** Copy or append a file into another. Optional permissions (by default [0o644]) concern of course the target.
    -- Adapted from {{:http://www.enseignement.polytechnique.fr/profs/informatique/Didier.Remy/system/camlunix/fich.html}Xavier Leroy and Didier Remy's OS course, Chapter 2}. *)
let file_copy_or_append ?(flag=Unix.O_TRUNC) ?(buffer_size=8192) ?perm input_name output_name =
  let perm = match perm with Some x -> x | None -> (Unix.stat input_name).Unix.st_perm in
  let buffer = String.create buffer_size in
  let fd_in  = Unix.openfile input_name  [Unix.O_RDONLY] 0 in
  let fd_out = Unix.openfile output_name [Unix.O_WRONLY; Unix.O_CREAT; flag] perm in
  let rec copy_loop () =
   match Unix.read fd_in buffer 0 buffer_size with
     0 -> ()
   | r -> ignore (Unix.write fd_out buffer 0 r); copy_loop () in
  copy_loop ();
  Unix.close fd_in;
  Unix.close fd_out
;;

(** Copy a file into another. Defaults are [buffer_size=8192] and [perm=0o644]. Permissions are used
    only if the target file must be created. *)
let file_copy   = file_copy_or_append ~flag:Unix.O_TRUNC  ;;

(** Append a file into another. Defaults are [buffer_size=8192] and [perm=0o644]. Permissions are used
    only if the target file must be created. *)
let file_append = file_copy_or_append ~flag:Unix.O_APPEND ;;

(** Try to rename or copy-and-unlink the source file. *)
let file_move input_name output_name =
  try  (* try to rename *)
    Unix.rename input_name output_name
  with (* else copy and unlink *)
    Unix.Unix_error (_,"rename",_) ->
       begin
        file_copy input_name output_name;
        Unix.unlink input_name;
       end
;;

(** Write or rewrite the file with the given content.
    If the file does not exists, it is created with the given permission
   (set by default to [0o644]). *)
let put ?(perm=0o644) (fname:filename) (x:content) : unit =
  let fd = (Unix.openfile fname [Unix.O_CREAT; Unix.O_WRONLY; Unix.O_TRUNC] perm) in
  let n = String.length x in
  ignore (Unix.write fd x 0 n);
  (Unix.close fd)
;;

(** Alias for [put]. *)
let rewrite = put;;

(** Similar to the function [put] described above, but the content is {b appended} instead of rewrited.
    If the file doesn't exists, it is created with the given permissions (set by default to [0o644]). *)
let append ?(perm=0o644) (fname:filename) (x:content) =
  let fd = (Unix.openfile fname [Unix.O_CREAT; Unix.O_WRONLY; Unix.O_APPEND] perm) in
  let n  = String.length x in
  ignore (Unix.write fd x 0 n);
  (Unix.close fd)
;;

(** Return the {b whole} content (caution!) of the file
    as a string. Use only for small files.
    Great for making pipelines. For instance,
    the following pipeline catches the first line of [/etc/fstab] containing
    the substring "hda1":
{[# "/etc/fstab" => ( cat || String.to_list || Str.grep ".*hda1.*" || hd ) ]}*)
let rec cat (fname:filename) =
  let fd = (Unix.openfile fname [Unix.O_RDONLY] 0o644) in
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

(** Create a temporary directory in a parent directory using a random name in the range [0..2^30].
    By default:
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

(** More safe (quite paranoic) functions using the [TMPDIR] environment variable
    and implemented as [Filename.open_temp] wrappers. *)
module TMPDIR = struct

let default_prefix = (Filename.basename Sys.executable_name)^".";;

let rec open_temp
  ?(perm=0o644)
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
 ?(perm=0o644)
 ?(prefix=default_prefix)
 ?(suffix="") () =
 let (filename,fd) = open_temp ~perm ~prefix ~suffix () in
 (Unix.close fd);
 filename

end;;

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

(** [iter_dir f dirname] iterate the function [f] on each entry of the directory [dirname].
-- From {{:http://www.enseignement.polytechnique.fr/profs/informatique/Didier.Remy/system/camlunix/fich.html}Xavier Leroy and Didier Remy's OS course, Chapter 2}. *)
let iter_dir f dirname =
      let d = Unix.opendir dirname in
      try while true do f (Unix.readdir d) done
      with End_of_file -> Unix.closedir d
;;


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


(** Process status printers; {b examples}:
{[# Process_status.printf "The result is '%s'\n" (snd (run "unexisting-program"));;
The result is 'Unix.WEXITED 127'
  : unit = ()

# Process_status.printf "The result is '%s'\n" (snd (run "ls"));;
The result is 'Unix.WEXITED 0'
 : unit = () ]} *)
module Process_status = PervasivesExtra.Printers0 (struct
    type t = Unix.process_status
    let string_of = function
    | Unix.WEXITED   code   -> (Printf.sprintf "Unix.WEXITED %d" code)
    | Unix.WSIGNALED signal -> (Printf.sprintf "Unix.WSIGNALED %d" signal)
    | Unix.WSTOPPED  signal -> (Printf.sprintf "Unix.WSTOPPED %d" signal)
    end);;

(** A {e command} is something understandable by the shell. *)
type command = string;;

(** A {e program} is a file binary (which will be found by the system in [PATH]). *)
type program = string;;

(** Search the directory containing the executable. Candidates are taken from the environment variable [PATH].
    The result [None] means not found. {b Examples}:
{[# UnixExtra.is_executable_in_PATH "ls" ;;
  : string option = Some "/bin"

# UnixExtra.is_executable_in_PATH "foo" ;;
  : string option = None
]} *)
let is_executable_in_PATH p =
 let is_there_an_executable p d =
   let filelist = Array.to_list (Sys.readdir d) in
   (List.mem p filelist) && (test_access ~x:() (d^"/"^p))
 in
 let dirs = StringExtra.split ~d:':' (Sys.getenv "PATH") in
 try Some(List.find (is_there_an_executable p) dirs)
 with Not_found -> None

(** Run Unix.system with the given argument, and raise exception in case of failure;
    return unit on success. *)
let system_or_fail ?(hide_output=false) ?(hide_errors=false) command =
  let suffix1 = if hide_output then " 1>/dev/null" else "" in
  let suffix2 = if hide_errors then " 2>/dev/null" else "" in
  let command = Printf.sprintf "%s%s%s" command suffix1 suffix2 in
  match Unix.system command with
  | Unix.WEXITED 0   -> ()
  | Unix.WEXITED n   -> failwith (Printf.sprintf "Unix.system: the process exited with %i" n)
  | Unix.WSIGNALED _
  | Unix.WSTOPPED _  -> failwith "Unix.system: the process was signaled or stopped"
;;

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
 ?pseudo
 ?(forward = [Sys.sigint; Sys.sigabrt; Sys.sigquit; Sys.sigterm; Sys.sigcont])
 ?register_pid
 program arguments =

 let (stdin,  stdin_must_be_closed )  = Source.to_file_descr stdin in
 let (stdout, stdout_must_be_closed)  = Sink.to_file_descr stdout  in
 let (stderr, stderr_must_be_closed)  = Sink.to_file_descr stderr  in

 let events = new_waiting_events () in
 let name = match pseudo with None -> program | Some name -> name in
 let argv = (Array.of_list (name :: arguments)) in
 let child_pid = (Unix.create_process program argv stdin stdout stderr) in
 (match register_pid with None -> () | Some f -> f child_pid);
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

(** High-level result: (code, stdout, stderr) *)
type process_result = (int * string * string) ;;

(** Similar to [create_process_and_wait], but the results on endpoints [stdout] and [stderr] are converted
    in strings and returned.
    However, if the optional parameters [stdout] and [stderr] are provided, their corresponding string in the result
    will be empty. *)
let create_process_and_wait_then_get_result ?stdin ?stdout ?stderr ?pseudo ?forward ?register_pid (program:program) (argv_list:string list) =
 begin
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
 let code =
    try  create_process_and_wait ?stdin ~stdout ~stderr ?pseudo ?forward ?register_pid program argv_list
    with _ -> (-1)
 in
 let stdout_string = stdout_string_maker () in
 let stderr_string = stderr_string_maker () in
 (code,stdout_string,stderr_string)
 end
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


(** A Unix future is a future containing the exit code and the two strings outcoming from stdout and stderr.
    The negative exit code (-1) means that the process didn't well exited. *)
type future = (int * string * string) Future.t ;;

(** Similar to {!val:UnixExtra.future}, but with a continuation executed {b within} the thread.
    The default for [forward] here is the empty list [[]]. *)
let kfuture ?stdin ?stdout ?stderr ?pseudo ?(forward=[]) ?register_pid (program:program) (argv_list:string list) k =
 begin
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
      let code =
        try  create_process_and_wait ?stdin ~stdout ~stderr ?pseudo ~forward ?register_pid program argv_list
        with _ -> (-1)
      in
      let stdout_string = stdout_string_maker () in
      let stderr_string = stderr_string_maker () in
      (k code stdout_string stderr_string)
     end) () in
 future
 end
;;

(** Create a {!type:UnixExtra.future} that you can manage as usual with functions of the module {!Future}. *)
let future ?stdin ?stdout ?stderr ?pseudo ?(forward=[]) ?register_pid program argv_list =
 kfuture ?stdin ?stdout ?stderr ?pseudo ~forward ?register_pid program argv_list (fun x y z -> (x,y,z))

(** With the {b content} provided by the user, a script file is created on the fly, executed and finally removed.
    The result is a 3-uple with the exit code and the two strings outcoming from stdout and stderr. *)
let script ?stdin ?stdout ?stderr ?pseudo ?(forward=[]) ?register_pid (content:content) (argv_list:string list) : (int * string * string) =
 begin
 let program = temp_file ~perm:0o755 ~suffix:".sh" ~content () in
 try
  let f = future ?stdin ?stdout ?stderr ?pseudo ~forward ?register_pid program argv_list in
  let result = Future.touch f in
  (Unix.unlink program);
  result
 with e -> ((Unix.unlink program); raise e)
 end
;;


(** [does_process_exist pid] return true if and only if the [pid] is alive in the system. *)
external does_process_exist : int -> bool = "does_process_exist_c";;
let is_process_alive = does_process_exist


module Process = struct

 type process_status =
 | WUNCHANGED        (* Used when non-blocking calls with WNOHANG return immediately without value *)
 | WEXITED of int    (* The process terminated normally by exit; the argument is the return code. *)
 | WSIGNALED of int  (* The process was killed by a signal; the argument is the signal number.    *)
 | WSTOPPED of int   (* The process was stopped by a signal; the argument is the signal number.   *)
 | WCONTINUED        (* The process was resumed *)

 type wait_flag =
 | WNOHANG           (* do not block if no child has died yet, but immediately return with a pid equal to 0. *)
 | WUNTRACED         (*	report also the children that receive stop signals. *)
 | WCONTINUE         (*  report also if the children resume *)

 external waitpid : wait_flag list -> int -> int * process_status = "waitpid_c"

 include PervasivesExtra.Printers0 (struct
    type t = process_status
    let string_of = function
    | WUNCHANGED       -> (Printf.sprintf "Process.WUNCHANGED")
    | WEXITED   code   -> (Printf.sprintf "Process.WEXITED %d" code)
    | WSIGNALED signal -> (Printf.sprintf "Process.WSIGNALED %d" signal)
    | WSTOPPED  signal -> (Printf.sprintf "Process.WSTOPPED %d" signal)
    | WCONTINUED       -> (Printf.sprintf "Process.WCONTINUED")
    end)

end

(** Return the current date formatted as a string like ["2010-06-24.17:34:25"].
    Dashes, dot and colons may be replaced by something else
    using the optional parameters. *)
let date ?(dash="-") ?(dot=".") ?(colon=":") ?no_time ?no_date () =
  let gmt = Unix.gmtime (Unix.time ()) in
  match no_time, no_date with
  | None, None ->
      Printf.sprintf "%4d%s%02d%s%2d%s%02d%s%02d%s%02d"
	(1900+gmt.Unix.tm_year) dash
	(1+gmt.Unix.tm_mon) dash
	(gmt.Unix.tm_mday)
	dot
	(gmt.Unix.tm_hour) colon
	(gmt.Unix.tm_min)  colon
	(gmt.Unix.tm_sec)
  | Some (), None ->
      Printf.sprintf "%4d%s%02d%s%2d"
	(1900+gmt.Unix.tm_year) dash
	(1+gmt.Unix.tm_mon) dash
	(gmt.Unix.tm_mday)

  | None, Some () ->
      Printf.sprintf "%02d%s%02d%s%02d"
	(gmt.Unix.tm_hour) colon
	(gmt.Unix.tm_min)  colon
	(gmt.Unix.tm_sec)
  | Some (), Some () -> invalid_arg "UnixExtra.date: strangely called with ~no_time:() and ~no_date:()"


(** Resolve a symbolic link if the argument is a symbolic link, otherwise
   return the argument (identity). {b Example}:
{[
# resolve_symlink "/initrd.img" ;;
  : string = "//boot/initrd.img-2.6.32-24-generic"

# resolve_symlink "/not/existing/file" ;;
  : string = "/not/existing/file"
]}
*)
let resolve_symlink ?(max_hops=64) filename =
 let rec loop max_hops filename =
  begin try
    if max_hops <= 0 then filename else
    let target = Unix.readlink filename in
    let target =
      (match (Filename.is_relative target) with
      | true ->
	  let dir = Filename.dirname filename in
	  Printf.sprintf "%s/%s" dir target
      | false -> target
      )
    in
    if target = filename then target else (loop (max_hops-1) target)
   with _ -> filename
  end
 in loop max_hops filename

let is_symlink filename =
 try
  ignore (Unix.readlink filename);
  true
 with _ -> false
