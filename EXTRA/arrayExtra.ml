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

(** Additional features for the standard module [Array].

{b Usage}:
-    {[ open ArrayExtra;; ]}
-    {[ module Array = ArrayExtra.Array;; ]}
The previous phrases are equivalent and allow you to access to additional features for arrays.

You can give a look to the {!ArrayExtra.Extra} module documentation for more informations on these features.
*)

(** Extra definitions for arrays. *)
module Extra = struct

(** Equivalent to the standard [Array.of_list] but the list is not scanned twice. The function raises [Invalid_argument]
    if the real length of the list differs from the announced. *)
let of_known_length_list ?(reversing=false) len = function
 | []    -> [||]
 | x::xs -> let a = Array.create len x in
     if reversing then
      (let rec fill i = function
       | [] -> (if i=(-1) then a else invalid_arg "unexpected list length (overstated size)")
       | x::xs -> (try a.(i) <- x with _ -> invalid_arg "unexpected list length (understated size)"); fill (i-1) xs 
       in fill (len-2) xs)
     else
      (let rec fill i = function
      | [] -> (if i=len then a else invalid_arg "unexpected list length (overstated size)")
      | x::xs -> (try a.(i) <- x with _ -> invalid_arg "unexpected list length (understated size)"); fill (i+1) xs 
      in fill 1 xs) 

end;; (* module Extra *)

(** Redefinition of the standard [Array]. *)
module Array = struct
  include Array;;
  include Extra;;
end;;
