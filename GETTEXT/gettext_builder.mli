(* This file is part of our reusable OCaml BRICKS library
   Copyright (C) 2009, 2010  Luca Saiu

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


(** In order to build translators, we just need a text domain and a directory holding
    translated strings: *)
module type TextDomainAndDirectory = sig
  val text_domain : string;;
  val directory : string;;
end;;

(** A Gettext module provides translation functions: *)
module type Gettext = sig
  (** Translate a string: *)
  val s_ : string -> string;;

  (** Translate a format string: *)
  val f_ : (('a, 'b, 'c) format) -> (('a, 'b, 'c) format);;
end;;

(** Given the information above, we provide a Gettext module: *)
module Make : functor (TheTextDomainAndDirectory : TextDomainAndDirectory) -> Gettext;;

(** "Low-level" interface: given a text domain and a string in English, return its
    translated version.  Before using this you must have called the functor with
    the same text domain and a suitable directory. *)
val dgettext : string -> string -> string;;
