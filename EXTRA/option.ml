(* This file is part of our reusable OCaml BRICKS library
   Copyright (C) 2009  Jean-Vincent Loddo

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

let extract ?(failwith_msg="Option.extract") ?(fallback=(fun () -> failwith failwith_msg)) =
 function
  | None   -> fallback ()
  | Some x -> x

let extract_or xo y = match xo with
 | Some x -> x
 | None   -> y

let map f = function None -> None | Some x -> Some (f x)
let bind x f = match x with None -> None | Some x -> (f x)
let return x = Some x
let iter f = function None -> () | Some x -> (f x)

let of_fallible_application ?(fallback=fun _ _ -> ()) f x =
 try Some (f x) with e -> ((fallback e x); None)

include PervasivesExtra.Printers1 (struct
  type 'a t = 'a option
  let string_of string_of_alpha = function
  | None   -> "None"
  | Some x -> "Some "^(string_of_alpha x)
 end)

let extract_from_list ?(acc=[]) xs =
 let rec loop = function
 | [] -> acc
 | None::xs -> (loop xs)
 | (Some x)::xs -> x::(loop xs)
 in
 loop xs

let of_bool = function
 | false -> None
 | true  -> Some ()

let to_bool = function
 | None   -> false
 | Some _ -> true

let to_list = function None -> [] | Some x -> [x]
