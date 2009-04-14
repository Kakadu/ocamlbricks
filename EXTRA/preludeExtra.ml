(* This file is part of our reusable OCaml BRICKS library
   Copyright (C) 2007  Luca Saiu

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

(* Authors:
 * - Luca Saiu: initial version
 * - Jean-Vincent Loddo: functors for printers
 *)

(** Additional features for the standard module [Pervasives].
    Open this module in order to use printing function such as [print_string] in
    {e synchronous} mode. *)

(** Extra definitions. *)
module Extra = struct
  (** Print something onto stdout, then immediately flush the buffer. This may be
      slower but allows to print without waiting for automatic flushes, which are
      very infrequent when more than one process is concurrently writing to the
      same channel *)
  let print_char c = Pervasives.print_char c; flush stdout
  let print_string s = Pervasives.print_string s; flush stdout
  let print_int i = Pervasives.print_int i; flush stdout
  let print_float f = Pervasives.print_float f; flush stdout
  let print_endline s = Pervasives.print_endline s; flush stdout
  let print_newline () =  Pervasives.print_newline (); flush stdout

  (** Make standard printers for a non-polymorphic type. *)
  module Printers0 (M:sig type t val string_of : t -> string end) = struct
   include M
   let print              x = (Printf.printf  "%s"   (M.string_of x))
   let prerr              x = (Printf.eprintf "%s"   (M.string_of x))
   let print_endline      x = (Printf.printf  "%s\n" (M.string_of x)); (flush stdout)
   let prerr_endline      x = (Printf.eprintf "%s\n" (M.string_of x)); (flush stderr)
   let fprintf outch frmt x = (Printf.fprintf outch  frmt (M.string_of x))
   let eprintf       frmt x = (Printf.fprintf stderr frmt (M.string_of x))
   let printf        frmt x = (Printf.printf         frmt (M.string_of x))
   let sprintf       frmt x = (Printf.sprintf        frmt (M.string_of x))
  end;;

  (** Make standard printers for a polymorphic type. *)
  module Printers1 (M:sig type 'a t val string_of : ('a->string) -> 'a t -> string end) = struct
   include M
   let print         string_of_alpha x = (Printf.printf  "%s"   (M.string_of string_of_alpha x))
   let prerr         string_of_alpha x = (Printf.eprintf "%s"   (M.string_of string_of_alpha x))
   let print_endline string_of_alpha x = (Printf.printf  "%s\n" (M.string_of string_of_alpha x)); (flush stdout)
   let prerr_endline string_of_alpha x = (Printf.eprintf "%s\n" (M.string_of string_of_alpha x)); (flush stderr)
   let fprintf       string_of_alpha outch frmt x = (Printf.fprintf outch  frmt (M.string_of string_of_alpha x))
   let eprintf       string_of_alpha       frmt x = (Printf.fprintf stderr frmt (M.string_of string_of_alpha x))
   let printf        string_of_alpha       frmt x = (Printf.printf         frmt (M.string_of string_of_alpha x))
   let sprintf       string_of_alpha       frmt x = (Printf.sprintf        frmt (M.string_of string_of_alpha x))
  end;;

end;; (* module Extra *)

(** Redefinition of module [Pervasives]. *)
module Prelude = struct
  (* ``include Pervasives;;'' is not needed in this case, of course *)
  include Extra;;
end;;
