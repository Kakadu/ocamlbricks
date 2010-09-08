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

(* Do not remove the following comment: it's an ocamldoc workaround. *)
(** *)

(** Reads a given directory, thus select and convert names. Returns the list of formatted names. *)
let readdir_as_list
  ?only_directories
  ?only_not_directories
  ?(name_filter:(string->bool)=(fun x -> true))
  ?(name_converter:(string->string)=(fun x->x))
  (dir:string) =
  try begin
    let filelist  = (Array.to_list (Sys.readdir dir)) in
    let first_filter =
      match only_directories, only_not_directories with
      | None, None    -> (fun x -> true)
      | Some (), None -> Sys.is_directory
      | None, Some () -> (fun x -> not (Sys.is_directory x))
      | Some (), Some () -> invalid_arg "SystExtra.readdir_as_list: ?only_directories and ?only_not_directories both set."
    in
    let safe_name_filter = (fun name -> (try (name_filter name) with _ -> false)) in
    let selected_items =
      List.filter (fun x -> (first_filter (dir^"/"^x)) && (safe_name_filter x)) filelist
    in
    List.map name_converter selected_items
  end with
  | (Invalid_argument msg) as e -> raise e
  |  _ -> []

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
