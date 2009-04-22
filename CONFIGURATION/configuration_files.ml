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


open UnixExtra;;

(** This module contains a simple implementation of application-wise configuration
    files, implemented as shell scripts. Configuration files are looked for (by
    default) in "standard" places like /etc, the user's home directory and the
    shell environment.
    There is a priority:
    - System-wise files in /etc
    - User's files in ~
    - The shell environment at application startup time. *)

(** An alist is just a list of pairs: *)
type 'a alist =
    (string * 'a) list;;

(** For each variable bound in the shell environment echo its name and its value,
one binding per line: *)
let output_of_environment variables =
  let command_line =
    List.fold_left
      (fun string variable ->
        (* Print a line with: the variable name, a space, and its value,
           IF the variable is defined in the environment; otherwise don't
           print anything, and let the configuration file binding (if any)
           take precedence: *)
        Printf.sprintf
          "%s if test -n \"$%s\"; then echo %s \"$%s\"; fi; "
          string variable variable variable)
      ""
      variables in
  (* Printf.printf "The command line is\n%s\n" command_line; *)
  let (output, exit_code) = Unix.run command_line in
  assert(exit_code = Unix.WEXITED 0);
  (* Printf.printf "The output is:\n-------------------------\n%s\n-------------------------\n" output; *)
  output;;

(** Evaluate the given file, then 'echo' each variable name and its value, one variable
    per line: *)
let output_of_file_name file_name variables =
  try
    let source_command_line =
      (* This is very important: dash does not support "source", you have to use the
         less readable, but more portable, ".": *)
      Printf.sprintf "set -e; (. %s 2> /dev/null &&" file_name in
    let command_line =
      List.fold_left
        (fun string variable ->
          (* Print a line with: the variable name, a space, and its value *)
          Printf.sprintf "%s echo %s $%s && " string variable variable)
        source_command_line
        variables in
    let command_line = command_line ^ " true) 2> /dev/null" in
    (* Printf.printf "The command line is %s\n" command_line; *)
    let (output, exit_code) = Unix.run command_line in
    if not (exit_code = Unix.WEXITED 0) then
      failwith ("Failed when source'ing the configuration file " ^ file_name)
    else begin
      (* Printf.printf "The output is:\n-------------------------\n%s\n-------------------------\n" output; *)
      output;
    end
  with _ -> begin
    (* Printf.printf "WARNING: could not source %s\n" file_name; *)
    "";
  end;;

(** Convert an output into a list of rows, where each row is a list of
    strings: first the variable name, then the value, possibly made of several
    tokens: *)
let matrix_of_output output =
  StringExtra.Text.Matrix.of_string output;;

(** Extract only the variable names from the matrix, disregarding values: *)
let variables_of_matrix matrix =
  List.map
    (fun row -> match row with
    | variable :: _ -> variable
    | _ -> assert false) (* no line should be empty *)
    matrix;;

(** Turn a matrix into an alist mapping each variable name into a value; each 
    variable value (as a list of strings) is passed to the given function to
    obtain the value which is bound in the returned environment. Variables for
    which the given function fails are simply ignored: *)
let alist_of_matrix row_to_element matrix =
  let result = ref [] in
  List.iter
    (fun row ->
      match row with
      | (variable :: values) ->
          (try result := (variable, (row_to_element values)) :: !result; with _ -> ())
      | _ ->
          assert false)
    matrix;
  !result;;

(** Turn a matrix into an alist mapping each variable name into a value; each 
    variable value (as a single string, with token separated by a single space)
    is passed to the given function to obtain the value which is bound in the
    returned environment. Variables for which the given function fails are simply
    ignored: *)
let scalar_alist_of_matrix string_to_element =
  alist_of_matrix
    (fun values -> string_to_element (StringExtra.Fold.blankcat values));;

(** Turn a matrix into an alist mapping each variable name into the list of
    the tokens of its value: *)
let list_alist_of_matrix =
  alist_of_matrix (fun values -> values);;

(** Turn a matrix into an alist mapping each variable name into the string
    containing its value (tokens concatenated into a string, separated by
    single spaces): *)
let string_alist_of_matrix =
  scalar_alist_of_matrix (fun string -> string);;

(** Turn a matrix into an alist mapping each variable name with an integer
    value into the integer. Non-integer-valued variables are ignored: *)
let int_alist_of_matrix =
  scalar_alist_of_matrix int_of_string;;

(** Turn a matrix into an alist mapping each variable name with an float
    value into the float. Non-float-valued variables are ignored: *)
let float_alist_of_matrix =
  scalar_alist_of_matrix float_of_string;;

(** Turn a matrix into an alist mapping each variable name with an bool
    value into the bool. Non-bool-valued variables are ignored: *)
let bool_alist_of_matrix =
  scalar_alist_of_matrix bool_of_string;;

(** Turn a matrix into a "tuple of alists", which henceforth means
    an alist of strings, an alist of ints, an alist of floats, an
    alist of bools, and an alist of lists of strings; as usual, values
    of the "wrong" type are ignored: *)
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
                        
(** Turn a *file* into a tuple of alists: *)
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

(** Make a configuration object from a list of file name or a software name;
    in the latter case the configuration files have "reasonable" default
    names: *)
class configuration =
  fun ?software_name
      ?file_names
      ~variables
      ?(read_environment=true)
      () ->
let file_names =
  match file_names, software_name with
  | None, None ->
      failwith "either ~software_name or ~file_names should be passed"
  | (Some file_names), None ->
      file_names
  | None, (Some software_name) ->
      [ Printf.sprintf "/etc/%s/%s.conf" software_name software_name;
        Printf.sprintf "~/.%s/%s.conf" software_name software_name ]
  | (Some _), (Some _) ->
      failwith "you should pass exactly one of ~software_name and ~file_names" in
object(self)
  (* Associative containers used for efficient access, after initialization: *)
  val string_hashmap = new Hashmap.hashmap ();
  val int_hashmap = new Hashmap.hashmap ();
  val float_hashmap = new Hashmap.hashmap ();
  val bool_hashmap = new Hashmap.hashmap ();
  val list_hashmap = new Hashmap.hashmap ();

  initializer
    (* First execute all configuration files in the correct order, and merge the
       bindings: *)
    let alists =
      List.fold_left
        (fun accumulator file_name ->
          merge_alists accumulator (alists_of_file file_name variables))
        ([], [], [], [], [])
        file_names in
    (* If we also want to access the shell environment, then look it up, and
       give it precedence over configuration files: *)
    let (string_alist, int_alist, float_alist, bool_alist, list_alist) =
      if read_environment then
        let environment_output = output_of_environment variables in
        merge_alists alists (alists_of_output environment_output variables)
      else
        alists in
    (* Finally convert the bindings from alists into hashes, for efficient access: *)
    string_hashmap#add_list string_alist;
    int_hashmap#add_list int_alist;
    float_hashmap#add_list float_alist;
    bool_hashmap#add_list bool_alist;
    list_hashmap#add_list list_alist;
    
  (** Lookup a variable of the given type: *)
  method lookup_string = string_hashmap#lookup
  method lookup_int = int_hashmap#lookup
  method lookup_float = float_hashmap#lookup
  method lookup_bool = bool_hashmap#lookup
  method lookup_list = list_hashmap#lookup
    
  (** Aliases for accessors: *)
  method string = self#lookup_string
  method int = self#lookup_int
  method float = self#lookup_float
  method bool = self#lookup_bool
  method list = self#lookup_list
end;; (* class *)

(* (\* Example *\) *)
(* let q = *)
(* new configuration *)
(*   ~file_names:["~luca/working/ocamlbricks/MYSETTINGS"; "~luca/working/ocamlbricks/MYSETTINGS2"] *)
(*   (\* ~software_name:"marionnet" *\) *)
(*   ~variables:["ZZZ"; "fortytwo";] *)
(*   ~read_environment:true *)
(*   ();; *)

(* Printf.printf ">%s<\n" (q#string "ZZZ");; *)
(* Printf.printf ">%i<\n" (q#int "fortytwo");; *)
(* Printf.printf ">%f<\n" (q#float "fortytwo");; *)
