(* This file is part of our reusable OCaml BRICKS library
   Copyright (C) 2008  Luca Saiu

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


open PreludeExtra.Prelude;; (* We want synchronous terminal output *)
open UnixExtra;;
open StringExtra;;

(* To do: move this into UnixExtra or something like that: *)
(** Run system with the given argument, and raise exception in case of failure;
    return unit on success. *)
let system_or_fail command_line =
  Printf.printf "Executing \'%s\'...\n" command_line;
  flush_all ();
  match Unix.system command_line with
    Unix.WEXITED 0 ->
      ()
  | Unix.WEXITED n ->
      failwith (Printf.sprintf "Unix.system: the process exited with %i" n)
  | Unix.WSIGNALED _ | Unix.WSTOPPED _ ->
      failwith "Unix.system: the process was signaled or stopped";;

let output_of_environment variables =
  let command_line =
    List.fold_left
      (fun string variable ->
        (* Print a line with: the variable name, a space, and its value *)
(** To do: this must be changed so that $%s is only echo'ed if it's defined. *)
        Printf.sprintf "%s echo %s $%s; " string variable variable)
      ""
      variables in
  let (output, exit_code) = Unix.run command_line in
  assert(exit_code = Unix.WEXITED 0);
  Printf.printf "The output is:\n-------------------------\n%s\n-------------------------\n" output;
  output;;

let output_of_file_name file_name variables =
  let source_command_line =
    Printf.sprintf "set -e; (source %s &&" file_name in
  let command_line =
    List.fold_left
      (fun string variable ->
        (* Print a line with: the variable name, a space, and its value *)
        Printf.sprintf "%s echo %s $%s && " string variable variable)
      source_command_line
      variables in
  let command_line = command_line ^ " true)" in
  Printf.printf "The command line is %s\n" command_line;
  let (output, exit_code) = Unix.run command_line in
  if not (exit_code = Unix.WEXITED 0) then
    failwith ("Failed when source'ing the configuration file " ^ file_name)
  else begin
    Printf.printf "The output is:\n-------------------------\n%s\n-------------------------\n" output;
    output;
  end;;

let matrix_of_output output =
  String.Text.Matrix.of_string output;;

let variables_of_matrix matrix =
  List.map
    (fun row -> match row with
                | variable :: _ -> variable
                | _ -> assert false)
    matrix;;

let alist_of_matrix row_to_element matrix =
  let result = ref [] in
  List.iter
    (fun row ->
      match row with
      | (variable :: values) -> begin
          try
            result := (variable, (row_to_element values)) :: !result;
          with _ ->
            ()
        end
      | _ ->
          assert false)
    matrix;
  !result;;

let scalar_alist_of_matrix string_to_element=
  alist_of_matrix
    (fun values -> string_to_element (String.Fold.blankcat values));;

let list_alist_of_matrix =
  alist_of_matrix (fun values -> values);;

let string_alist_of_matrix =
  scalar_alist_of_matrix (fun string -> string);;

let int_alist_of_matrix =
  scalar_alist_of_matrix int_of_string;;

let float_alist_of_matrix =
  scalar_alist_of_matrix float_of_string;;

let bool_alist_of_matrix =
  scalar_alist_of_matrix bool_of_string;;

let alists_of_output output variables =
  let matrix = matrix_of_output output in
  let string_alist : (string * string) list =
    string_alist_of_matrix matrix in
  let int_alist : (string * int) list =
    int_alist_of_matrix matrix in
  let float_alist : (string * float) list =
    float_alist_of_matrix matrix in
  let bool_alist : (string * bool) list =
    bool_alist_of_matrix matrix in
  let list_alist : (string * (string list)) list =
    list_alist_of_matrix matrix in
  string_alist, int_alist, float_alist, bool_alist, list_alist;;
  
let alists_of_file file_name variables =
  let output = output_of_file_name file_name variables in
  alists_of_output output variables;;

(** Merge the two given alist groups; the latest one takes precedence: *)
let merge_alists alists1 alists2 =
  let (string_alist1, int_alist1, float_alist1, bool_alist1, list_alist1) =
    alists1 in
  let (string_alist2, int_alist2, float_alist2, bool_alist2, list_alist2) =
    alists2 in
  (string_alist1 @ string_alist2,
   int_alist1 @ int_alist2,
   float_alist1 @ float_alist2,
   bool_alist1 @ bool_alist2,
   list_alist1 @ list_alist2);;

(* To do: autogenerate a meta.ml file from META in buildsystem. This allows us
   to automatically get the package name (hence the configuration file name) 
   without supplying ~file_names, which will become optional. *)
class configuration =
  fun ~file_names
      ~variables
      ?(read_environment=true)
      () ->
object
  val string_hashmap = new Hashmap.hashmap ();
  val int_hashmap = new Hashmap.hashmap ();
  val float_hashmap = new Hashmap.hashmap ();
  val bool_hashmap = new Hashmap.hashmap ();
  val list_hashmap = new Hashmap.hashmap ();

  initializer
    let alists =
      List.fold_left
        (fun accumulator file_name ->
          merge_alists accumulator (alists_of_file file_name variables))
        ([], [], [], [], [])
        file_names in
    let (string_alist, int_alist, float_alist, bool_alist, list_alist) =
      if read_environment then
        let environment_output = output_of_environment variables in
        merge_alists alists (alists_of_output environment_output variables)
      else
        alists in
    string_hashmap#add_alist string_alist;
    int_hashmap#add_alist int_alist;
    float_hashmap#add_alist float_alist;
    bool_hashmap#add_alist bool_alist;
    list_hashmap#add_alist list_alist;

  method lookup_string = string_hashmap#lookup
  method lookup_int = int_hashmap#lookup
  method lookup_float = float_hashmap#lookup
  method lookup_bool = bool_hashmap#lookup
  method lookup_list = list_hashmap#lookup
end;;

let q =
  new configuration
    ~file_names:["MYSETTINGS"]
    ~variables:["ZZZ"]
    ();;
Printf.printf "%s\n" (q#lookup_string "ZZZ");;
