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


(** This simple program run as the famous {b grep} command but taking a regular expression in the [Str] O'Caml library format.
    The regular expression must be given as the first and unique argument. The parsed file is the standard input. *)

let main () =
 let re = Str.regexp (Sys.argv.(1)) in
 let rec boucle () =
  try begin
   let line = read_line () in
   (match Str.string_match re line 0 with
	| true  -> print_endline line
	| false -> ());
   boucle ()
  end with End_of_file -> ()
  in boucle ()
;;

main ()
;;


