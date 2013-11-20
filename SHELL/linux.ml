(* This file is part of ocamlbricks
   Copyright (C) 2013  Jean-Vincent Loddo
   Copyright (C) 2013  Université Paris 13

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

type pid = int

module Process = struct

 type stat = {
   pid          : int;            (* %d (1) *)
   comm         : string;         (* %s (2) *)
   state        : char;           (* %c (3) *)
   ppid         : int;            (* %d (4) *)
   pgrp         : int;            (* %d (5) *)
   session      : int;            (* %d (6) *)
   tty_nr       : int;            (* %d (7) *)
   tpgid        : int;            (* %d (8) *)
   flags        : int;            (* %u (%lu before Linux 2.6.22) (9) *)
   minflt       : int64;          (* %lu (10) *)
   cminflt      : int64;          (* %lu (11) *)
   majflt       : int64;          (* %lu (12) *)
   cmajflt      : int64;          (* %lu (13) *)
   utime        : int64;          (* %lu (14) *)
   stime        : int64;          (* %lu (15) *)
   cutime       : int64;          (* %ld (16) *)
   cstime       : int64;          (* %ld (17) *)
   priority     : int64;          (* %ld (18) *)
   nice         : int64;          (* %ld (19) *)
   num_threads  : int64;          (* %ld (20) *)
   itrealvalue  : int64;          (* %ld (21) *)
   starttime    : int64;          (* %llu (was %lu before Linux 2.6) (22) *)
   vsize        : int64;          (* %lu (23) *)
   rss          : int64;          (* %ld (24) *)
   rsslim       : int64 option;   (* %lu (25) *)
   startcode    : int64;          (* %lu (26) *)
   endcode      : int64;          (* %lu (27) *)
   startstack   : int64;          (* %lu (28) *)
   kstkesp      : int64;          (* %lu (29) *)
   kstkeip      : int64;          (* %lu (30) *)
   signal       : int64;          (* %lu (31) *)
   blocked      : int64;          (* %lu (32) *)
   sigignore    : int64;          (* %lu (33) *)
   sigcatch     : int64;          (* %lu (34) *)
   wchan        : int64 option;   (* %lu (35) *)
   nswap        : int64;          (* %lu (36) *)
   cnswap       : int64;          (* %lu (37) *)
   exit_signal  : int;            (* %d (since Linux 2.1.22) (38) *)
   processor    : int;            (* %d (since Linux 2.2.8)  (39) *)
   rt_priority  : int;            (* %u (since Linux 2.5.19; was %lu before Linux 2.6.22) (40) *)
   policy       : int;            (* %u (since Linux 2.5.19; was %lu before Linux 2.6.22) (41) *)
   delayacct_blkio_ticks : int64; (* %llu (since Linux 2.6.18) (42) *)
   guest_time   : int64;          (* %lu (since Linux 2.6.24) (43) *)
   cguest_time  : int64;          (* %ld (since Linux 2.6.24) (44) *)
 } (* type stat *)

 type stat_alias = stat
 type easy_stat = < pid:int; comm:string; state:char; ppid:int; pgrp:int; session:int; tty_nr:int; tpgid:int; other_fields:string >

 let stat_constructor
   pid comm state ppid pgrp session tty_nr tpgid flags minflt cminflt majflt cmajflt
   utime stime cutime cstime priority nice num_threads itrealvalue starttime vsize
   rss (rsslim:string) startcode endcode startstack kstkesp kstkeip signal blocked sigignore
   sigcatch (wchan:string) nswap cnswap exit_signal processor rt_priority policy
   delayacct_blkio_ticks guest_time cguest_time
   = let rsslim = try Some (Int64.of_string rsslim) with _ -> None in
     let wchan  = try Some (Int64.of_string wchan)  with _ -> None
     in
     { pid=pid; comm=comm; state=state; ppid=ppid; pgrp=pgrp; session=session; tty_nr=tty_nr;
       tpgid=tpgid; flags=flags; minflt=minflt; cminflt=cminflt; majflt=majflt; cmajflt=cmajflt;
       utime=utime; stime=stime; cutime=cutime; cstime=cstime; priority=priority; nice=nice;
       num_threads=num_threads; itrealvalue=itrealvalue; starttime=starttime; vsize=vsize;
       rss=rss; rsslim=rsslim; startcode=startcode; endcode=endcode; startstack=startstack;
       kstkesp=kstkesp; kstkeip=kstkeip; signal=signal; blocked=blocked; sigignore=sigignore;
       sigcatch=sigcatch; wchan=wchan; nswap=nswap; cnswap=cnswap; exit_signal=exit_signal;
       processor=processor; rt_priority=rt_priority; policy=policy; delayacct_blkio_ticks=delayacct_blkio_ticks;
       guest_time=guest_time; cguest_time=cguest_time;
       }

 let stat pid =
  let filename = Printf.sprintf "/proc/%d/stat" pid in
  try
    let ch = open_in filename in
    let result =
      try
	let obj = Scanf.fscanf ch
	 (* 0                           1                                      2                                       3                                     4               *)
         (* 1  2  3  4  5  6  7  8  9   0  1   2   3   4   5   6   7   8   9   0   1   2   3   4   5  6   7   8   9   0   1   2   3   4   5   6   7   8  9  0  1  2   3   4 *)
          "%d %s %c %d %d %d %d %d %u %Lu %Lu %Lu %Lu %Lu %Lu %Ld %Ld %Ld %Ld %Ld %Ld %Lu %Lu %Ld %s %Lu %Lu %Lu %Lu %Lu %Lu %Lu %Lu %Lu %s %Lu %Lu %d %d %u %u %Lu %Lu %Ld"
	  stat_constructor
	in
	Some obj
      with Scanf.Scan_failure(msg) ->
        (Printf.kfprintf flush stderr "Linux.stat: failed scanning file %s: %s\n" filename msg; None)
    in
    let () = close_in ch in
    result
  with _ -> None

 let easy_stat pid =
  let easy_stat_constructor pid comm state ppid pgrp session tty_nr tpgid other_fields =
    object
      method pid=pid;
      method comm=comm;
      method state=state;
      method ppid=ppid;
      method pgrp=pgrp;
      method session=session;
      method tty_nr=tty_nr;
      method tpgid=tpgid;
      method other_fields=other_fields;
    end
  in
  let filename = Printf.sprintf "/proc/%d/stat" pid in
  try
    let ch = open_in filename in
    let result =
      try
	let obj = Scanf.fscanf ch "%d %s %c %d %d %d %d %d %s@\n" easy_stat_constructor in
	Some obj
      with Scanf.Scan_failure(msg) ->
        (Printf.kfprintf flush stderr "Linux.easy_stat: failed scanning file %s: %s\n" filename msg; None)
    in
    let () = close_in ch in
    result
  with _ -> None


 let get_proc_PID_directories () =
  let xs = UnixExtra.Dir.to_list ~entry_kind:Unix.S_DIR "/proc/" in
  let ys = List.filter (fun x -> Sys.file_exists (Printf.sprintf "/proc/%s/stat" x)) xs  in
  let zs = ListExtra.filter_map (fun y -> try Some (int_of_string y) with _ -> None) ys in
  zs

 (* Make a multimap: ppid -> children *)
 module Int_key = struct type t = int let compare = Pervasives.compare end
 module Int_elt = Int_key
 module Parent_children_multimap
  : Multimap.S with
      type key = Int_key.t and
      type elt = Int_elt.t and
      type elt_set = SetExtra.Make(Int_elt).t
   = Multimap.Make(Int_key)(Int_elt)

 (* Functor making exported functions for both types (`stat' and `easy_stat')*)
 module Make_descendant_stats_functions
 (M:sig
      type stat
      val  stat : pid -> stat option
      val  ppid_prj : stat -> pid
      val  pid_prj  : stat -> pid
    end)
 = struct

  let get_stats () =
    let zs = get_proc_PID_directories () in
    ListExtra.filter_map (M.stat) zs

  module Pid_stat_map = MapExtra.Make (Int_key)

  let get_parent_children_multimap_and_stat_map () =
    let os = get_stats () in
    let ppid_pid_bindings  = List.map (fun o -> (M.ppid_prj o), (M.pid_prj o)) os in
    let pid_stat_bindings = List.map (fun o -> (M.pid_prj o), o) os in
    (* Make the multimap: ppid -> children *)
    let mmap = Parent_children_multimap.of_list (ppid_pid_bindings) in
    (* Make the map: ppid -> stat *)
    let map = Pid_stat_map.of_list (pid_stat_bindings) in
    (mmap, map)

  (* Note that this implementation is close but not strictly equivalent to map
      (with List.map) the function `stat' over the list resulting from `get_descendants'.
      Actually, with this version the files /proc/%s/stat are read *once*, not twice: *)
  let get_descendant_stats ?(pid=Unix.getpid ()) () =
    let mmap, map = get_parent_children_multimap_and_stat_map () in
    let rec loop ppid =
      let children = Parent_children_multimap.find_list ppid mmap in
      List.concat (List.map (fun pid -> pid::(loop pid)) children)
    in
    let descendants = loop pid in
    let precalculated_stat pid = Pid_stat_map.find pid map in
    List.map (precalculated_stat) descendants

  (* Optimized as `get_descendant_stats' (see the previous comment): *)
  let get_descendant_stats_as_forest ?(pid=Unix.getpid ()) () =
    let mmap, map = get_parent_children_multimap_and_stat_map () in
    let successors ppid = Parent_children_multimap.find_list ppid mmap in
    let (_pid, descendants_as_forest) = Forest.tree_of_acyclic_relation ~successors ~root:pid in
    let precalculated_stat pid = Pid_stat_map.find pid map in
    Forest.map (precalculated_stat) (descendants_as_forest)

  let get_parent_children_multimap () =
    let os = get_stats () in
    let ppid_pid_bindings  = List.map (fun o -> (M.ppid_prj o), (M.pid_prj o)) os in
    (* Make the multimap: ppid -> children *)
    let mmap = Parent_children_multimap.of_list (ppid_pid_bindings) in
    mmap

  let get_descendants ?(pid=Unix.getpid ()) () =
    let mmap = get_parent_children_multimap () in
    let rec loop ppid =
      let children = Parent_children_multimap.find_list ppid mmap in
      List.concat (List.map (fun pid -> pid::(loop pid)) children)
    in
    loop pid

  let get_descendants_as_forest ?(pid=Unix.getpid ()) () =
    let mmap = get_parent_children_multimap () in
    let successors ppid = Parent_children_multimap.find_list ppid mmap in
    let (_pid, forest) = Forest.tree_of_acyclic_relation ~successors ~root:pid in
    forest

  let get_children ?(pid=Unix.getpid ()) () =
    let mmap = get_parent_children_multimap () in
    Parent_children_multimap.find_list pid mmap

 end (* functor Make_descendant_stats_functions *)

 module Easy_stat =
  Make_descendant_stats_functions
    (struct
       type stat = easy_stat
       let  stat = easy_stat
       let ppid_prj x = x#ppid
       let pid_prj  x = x#pid
     end)

 let get_easy_stats                      = Easy_stat.get_stats
 let get_descendant_easy_stats           = Easy_stat.get_descendant_stats
 let get_descendant_easy_stats_as_forest = Easy_stat.get_descendant_stats_as_forest

 (* The following functions are based on `easy_stat' because with this type there are less
    chances of scan failures: *)
 let get_children                        = Easy_stat.get_children
 let get_descendants                     = Easy_stat.get_descendants
 let get_descendants_as_forest           = Easy_stat.get_descendants_as_forest

 module Full_stat =
  Make_descendant_stats_functions
    (struct
       type stat = stat_alias
       let  stat = stat
       let ppid_prj x = x.ppid
       let pid_prj  x = x.pid
     end)

 let get_stats                      = Full_stat.get_stats
 let get_descendant_stats           = Full_stat.get_descendant_stats
 let get_descendant_stats_as_forest = Full_stat.get_descendant_stats_as_forest

 module Kill_descendants = struct

 let still_alive_after_kill ~signal ~wait_delay pid_list =
  if pid_list = [] then [] (* return *) else (* continue *)
  let () = List.iter (fun pid -> try Unix.kill pid signal with _ -> ()) pid_list in
  (* Leave to the fathers the time to register the death of their children: *)
  let () = Thread.delay wait_delay in
  let alive_list = List.filter (UnixExtra.is_process_alive) pid_list in
  alive_list

 let killall ?(signal_sequence=[Sys.sigterm; Sys.sigint; Sys.sigcont; Sys.sigkill]) ?(wait_delay=0.1) ?(wait_delay_factor=2.) ?(retries=1) pids =
  let rec loop i wait_delay =
    let alive_list = pids in
    if i > retries then () (* abandon *) else
    let alive_list =
      List.fold_left
        (fun alive_list signal -> still_alive_after_kill ~signal ~wait_delay alive_list)
        (alive_list)
        (signal_sequence)
    in
    if alive_list = [] then () else loop (i+1) (wait_delay *. wait_delay_factor)
  in
  loop 1 wait_delay

 let kill_descendants
  ?sequential
  ?(wait_delay=0.1)
  ?(wait_delay_node_increase_factor=2.)
  ?(wait_delay_root_increase_factor=2.)
  ?(node_max_retries=1)
  ?(root_max_retries=1)
  ?signal_sequence
  ?(pid=Unix.getpid ())
  ()
  =
  let rec main_loop j wait_delay =
    if j > root_max_retries then () (* abandon *) else
    let f0 = get_descendant_stats_as_forest ~pid () in
    if Forest.is_empty f0 then () (* stop *) else (* continue *)
    (* The last launched will be the first killed: *)
    let f1 = Forest.sort (fun p1 p2 -> compare p2.starttime p1.starttime) f0 in
    (* The forest evaluation function: *)
    let eval x = function
      (* Leaf evaluation: do nothing, just provide the pid to the father *)
      | [] -> x.pid
      (* Node evaluation: kill children, then provide the pid to the father *)
      | x_children ->
	let () = killall ?signal_sequence ~wait_delay ~wait_delay_factor:(wait_delay_node_increase_factor) ~retries:(node_max_retries) x_children in
	x.pid
    in
    let backprop =
      match sequential with
      | None    -> Forest.backprop_parallel
      | Some () -> Forest.backprop
    in
    let children  = backprop eval f1 in
    let () = killall ?signal_sequence ~wait_delay ~wait_delay_factor:(wait_delay_node_increase_factor) ~retries:(node_max_retries) children in
    main_loop (j+1) (wait_delay *. wait_delay_root_increase_factor)
  in
  main_loop 1 (wait_delay)

 end (* Kill_descendants *)

 include Kill_descendants

end (* Process *)
