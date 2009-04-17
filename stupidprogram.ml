(* This is just a build system test. *)
print_string "Hello, world!\n";;

external does_process_exist : int -> bool = "does_process_exist_c";;

let pid = 16523;;
Printf.printf "Does the process with pid %i exist? %b\n" pid (does_process_exist pid);;
