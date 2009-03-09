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

(** Additional features for the standard library [Unix]. 
    Open this module in order to use the extended version of [Unix] instead of
    the standard one. *)

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

(** {2 Copying} *)

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


(** {2 Saving strings} *)

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

(** {2 Loading strings} *)

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


(** {2 Temporary files} *)

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

(** {2 Kind} *)

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

(** {2 Directories} *)

(** [iter_dir f dirname] iterate the function [f] on each entry of the directory [dirname].
-- From {{:http://www.enseignement.polytechnique.fr/profs/informatique/Didier.Remy/system/camlunix/fich.html}Xavier Leroy and Didier Remy's OS course, Chapter 2}. *)
let iter_dir f dirname =
      let d = Unix.opendir dirname in
      try while true do f (Unix.readdir d) done
      with End_of_file -> Unix.closedir d
;;


(** {3 Find} *)

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



(** {2 Password} *)

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


(** {2 Running} *)

(** A {e command} is a string. *)
type command = string;;

(** Returns the pair (output, exit-code) of the given system command. A string can be
    specified as input for the command. The flag [trace] (by default set to [false])
    permits to obtain some informations about the running on [stderr]. 

{b Examples}:
{[# run "ls /etc/*tab";;
  : string * Unix.process_status =
("/etc/crontab\n/etc/fstab\n/etc/inittab\n/etc/mtab\n/etc/quotatab\n", Unix.WEXITED 0)

# run ~input:"hello" "cat";;
  : string * Unix.process_status = ("hello", Unix.WEXITED 0)

# run ~input:"HELLO" "head -n 1 /etc/passwd /dev/stdin | cut -c-15";;
  : string * Unix.process_status =
("==> /etc/passwd\nat:x:25:25:Batc\n\n==> /dev/stdin \nHELLO\n", Unix.WEXITED 0)
]} *)
let run ?(trace:bool=false) ?(input:content="") (cmd:command) : string * Unix.process_status = 
  let script = temp_file ~perm:0o755 ~prefix:"script-" ~suffix:".sh" ~content:cmd ()  in 
  let output = temp_file ~perm:0o644 ~prefix:"script-" ~suffix:".output" () in 
  let (input_option,input_file) = if (input="") 
    then ("","")
    else 
      let name=(temp_file ~perm:0o644 ~prefix:"script-" ~suffix:".input" ()) in 
      begin
        put name input; 
        ((" <"^name),name) 
      end
  in 
  let code = Unix.system("bash -c " ^script^" >"^output^input_option) in 
  let str = (cat output) in
  begin
    if trace then begin 
      prerr_endline ("\n======> INPUT FILE: <<EOF\n"^input^"EOF"); 
      prerr_endline ("\n======> SCRIPT CONTENT: <<EOF\n"^cmd^"EOF");
      prerr_endline ("\n======> OUTPUT: <<EOF\n"^str^"EOF");
      ()
    end;
    Unix.unlink script; 
    Unix.unlink output;
    if (not (input="")) then (Unix.unlink input_file);  
    (str,code)
  end 
;;

(** As [run], but ignoring the exit-code. This function is
    simply a shortcut for the composition of [run] with [fst]. {b Examples}:
 
{[# shell "date";;
  : string = "ven avr 13 18:34:02 CEST 2007\n"

# String.Text.to_matrix (shell "wc -l /etc/*tab");;
  : string list list =
[["8"; "/etc/crontab"]; ["20"; "/etc/fstab"]; ["98"; "/etc/inittab"];
 ["11"; "/etc/mtab"]; ["127"; "/etc/naccttab"]; ["9"; "/etc/quotatab"];
 ["273"; "total"]]
]}*)
let shell ?(trace:bool=false) ?(input:string="") cmd = fst(run ~trace ~input cmd) 
;;


end;; (* module Extra *)

(** Redefinition of module [Unix]. *)
module Unix = struct
  include Unix;;
  include Extra;;
end;;
