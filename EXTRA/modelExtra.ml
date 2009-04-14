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


(** Additional features for the standard module [XXX].

{b Usage}:
-    {[ open XXXExtra;; ]}
-    {[ module XXX = XXXExtra.XXX;; ]}
The previous phrases are equivalent and allow you to access to additional features for XXX.

You can give a look to the {!XXXExtra.Extra} module documentation for more informations on these features.
*)

(** Extra definitions for XXX. *)
module Extra = struct

 (* Add your additional features here. *)

end;; (* module Extra *)


(** Redefinition of the standard [XXX]. *)
module XXX = struct
  include XXX;;
  include Extra;;
end;;
