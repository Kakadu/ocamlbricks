(* This file is part of our reusable OCaml BRICKS library
   Copyright (C) 2007  Jean-Vincent Loddo, Luca Saiu

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

(** Additional features for the standard module [Sys].

{b Usage}:
-    {[ open SysExtra;; ]}
-    {[ module Sys = SysExtra.Sys;; ]}
The previous phrases are equivalent and allow you to access to additional features for system operations.

You can give a look to the {!SysExtra.Extra} module documentation for more informations on these features.
*)


(** Extra definitions for system operations. *)
module Extra = struct

(** {2 Reading directories } *)
  
 (** Reads a given directory, thus select and convert names. Returns the list of formatted names. *)
let readdir_into_list ?(namefilter:(string->bool)=(fun x -> true)) ?(nameconverter:(string->string)=(fun x->x)) (dir:string) =
  try 
    let filelist  = (Array.to_list (Sys.readdir dir)) in
    let filter    = (fun n -> (try (namefilter n) with _ -> false)) in
    let selection = (List.filter filter filelist) in
    (List.map nameconverter selection)
  with _ -> [] 

(** {2 Rewriting files } *)

(** [put content filename] rewrite [filename] with the given [content] string.
    An optional [~callback] may be provided in order to catch the
    exception [(Sys_error msg)]. By default, the callback
    print the [msg] on [stderr] and exit from program with the exit code [1]. *)
let put =
 let std_callback msg = ((Printf.eprintf "%s" msg); exit 1) in
 fun ?(callback=std_callback) (content:string) (filename:string) ->
 (try
  let out_channel = open_out filename in
  (Printf.fprintf out_channel "%s" content);
  (close_out out_channel);
 with Sys_error msg -> callback msg)

(** {2 Signals} *)

(** Convert the signal in an integer as indicated by the unix command [kill -l]. *)
let int_of_signal = function
 | x when x=Sys.sigabrt   -> 6
 | x when x=Sys.sigalrm   -> 14
 | x when x=Sys.sigfpe    -> 8
 | x when x=Sys.sighup    -> 1
 | x when x=Sys.sigill    -> 4
 | x when x=Sys.sigint    -> 2
 | x when x=Sys.sigkill   -> 9
 | x when x=Sys.sigpipe   -> 13
 | x when x=Sys.sigquit   -> 3
 | x when x=Sys.sigsegv   -> 11
 | x when x=Sys.sigterm   -> 15
 | x when x=Sys.sigusr1   -> 10
 | x when x=Sys.sigusr2   -> 12
 | x when x=Sys.sigchld   -> 17
 | x when x=Sys.sigcont   -> 18
 | x when x=Sys.sigstop   -> 19
 | x when x=Sys.sigtstp   -> 20
 | x when x=Sys.sigttin   -> 21
 | x when x=Sys.sigttou   -> 22
 | x when x=Sys.sigvtalrm -> 26
 | x when x=Sys.sigprof   -> 27
 | x -> x


(** Convert the signal in a string as indicated by the unix command [kill -l]. *)
let string_of_signal = function
 | x when x=Sys.sigabrt -> "SIGABRT"
 | x when x=Sys.sigalrm -> "SIGALRM"
 | x when x=Sys.sigfpe  -> "SIGFPE"
 | x when x=Sys.sighup  -> "SIGHUP"
 | x when x=Sys.sigill  -> "SIGILL"
 | x when x=Sys.sigint  -> "SIGINT"
 | x when x=Sys.sigkill -> "SIGKILL"
 | x when x=Sys.sigpipe -> "SIGPIPE"
 | x when x=Sys.sigquit -> "SIGQUIT"
 | x when x=Sys.sigsegv -> "SIGSEGV"
 | x when x=Sys.sigterm -> "SIGTERM"
 | x when x=Sys.sigusr1 -> "SIGUSR1"
 | x when x=Sys.sigusr2 -> "SIGUSR2"
 | x when x=Sys.sigchld -> "SIGCHLD"
 | x when x=Sys.sigcont -> "SIGCONT"
 | x when x=Sys.sigstop -> "SIGSTOP"
 | x when x=Sys.sigtstp -> "SIGTSTP"
 | x when x=Sys.sigttin -> "SIGTTIN"
 | x when x=Sys.sigttou -> "SIGTTOU"
 | x when x=Sys.sigvtalrm -> "SIGVTALRM"
 | x when x=Sys.sigprof -> "SIGPROF"
 | x -> (string_of_int (int_of_signal x))

end;; (* module Extra *)


(** Redefinition of the standard [Sys]. *)
module Sys = struct
  include Sys;;
  include Extra;;
end;;
