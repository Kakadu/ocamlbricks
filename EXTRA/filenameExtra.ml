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
;;
