(*  This file is part of our reusable OCaml BRICKS library
    Copyright (C) 2010  Jean-Vincent Loddo

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.*)

(** Observe the module loading order. *)

(** Usage (in your <source>.ml):
{[
#load "log_module_loading_p4.cmo"
;;
]}

With this directive all files are prepended with the call:
{[
Log.printf "Loading module <filename>\n" ;;
]}
In this way you can observe the order of modules loaded by the application.
*)