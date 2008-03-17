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

(** All extra modules grouped in a single module. 
    Open this module in order to use the extended versions of: 
- [List]
- [String]
- [Str]
- [Unix]
- [Sys]
- [Filename]
- [Prelude]
 *)

 module List   = ListExtra.List;;
 module String = StringExtra.String;;
 module Str    = StrExtra.Str;;
 module Unix   = UnixExtra.Unix;;
 module Sys    = SysExtra.Sys;;
 module Filename = FilenameExtra.Filename;;
 module Prelude = PreludeExtra.Prelude;;
