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

(* Do not remove the following comment: it's an ocamldoc workaround. *)
(** *)

(** [add_extension_if_absent filename ext] append to the string [filename]
    the extension [ext] but only if the filename has no already an extension. 
    This operation just works on strings and doesn't modify anything in the filesystem.

{b Example}:
{[# add_extension_if_absent "foo" "txt";;
  : string = "foo.txt"

# add_extension_if_absent "foo.c" "txt";;
  : string = "foo.c"
]}*)
let add_extension_if_absent filename ext =
 try 
  let _ = (Filename.chop_extension filename) in 
  filename                      (* because the filename already has an extension *)
 with _ -> (filename^"."^ext)    (* because the filename has no extension *)

(** {b Example}: 
{[# get_extension "/tmp/aaa.bbb.ccc" ;;
  : string option = Some "ccc"

# get_extension "/tmp/aaa" ;;
  : string option = None
]}*)
let get_extension ?with_dot filename =
try
  let x = (Filename.chop_extension filename) in
  let a = String.length x in
  let b = String.length filename in
  (match with_dot with
   | None    -> Some (String.sub filename (a+1) (b-a-1))
   | Some () -> Some (String.sub filename a (b-a))
  )
with _ -> None

(** The default is the empty string. {b Examples}:
{[# get_extension_or_default "foo" ;;
  : string = ""

# get_extension_or_default "foo.txt" ;;
  : string = "txt"

# get_extension_or_default ~with_dot:() "foo.txt" ;;
  : string = ".txt"
]}
*)
let get_extension_or_default ?with_dot ?(default="") filename =
 match (get_extension ?with_dot filename) with
 | None -> default
 | Some r -> r

(** [Filename.concat] generalized to lists.  {b Examples}:
{[# concat_all ["aaa";"bbb";"ccc"] ;;
 : string = "aaa/bbb/ccc"
]} *)
let concat_list = String.concat "/"

let temp_dir ?temp_dir ?(prefix="") ?(suffix="") ?(perm=0o755) () =
  let result = Filename.temp_file ?temp_dir prefix suffix in
  Sys.remove result;
  Unix.mkdir result perm;
  Unix.chmod result perm; (* Yes, we insist because it seems necessary... *)
  result
