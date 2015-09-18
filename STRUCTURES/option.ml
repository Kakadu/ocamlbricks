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

type 'a t = 'a option   
   
let extract ?(failwith_msg="Option.extract") ?(fallback=(fun () -> failwith failwith_msg)) =
 function
  | None   -> fallback ()
  | Some x -> x

let extract_or xo y = match xo with
 | Some x -> x
 | None   -> y

let extract_or_force xo y = match xo with
 | Some x -> x
 | None   -> Lazy.force y

let extract_map_or xo f y = match xo with
 | Some x -> f x
 | None   -> y
 
let map f = function None -> None | Some x -> Some (f x)
let bind x f = match x with None -> None | Some x -> (f x)
let return x = Some x
let iter f = function None -> () | Some x -> (f x)
let join = function
| None -> None
| Some x -> x

(* Monadic definition: *)
let map2 f m1 m2  = bind m1 (function x1 -> map (f x1) m2)
let bind2 m1 m2 f = bind m1 (function x1 -> bind m2 (f x1))

let filter p x = bind x (fun x -> if p x then Some x else None)

let of_fallible_application ?(fallback=fun _ _ -> ()) f x =
 try Some (f x) with e -> ((fallback e x); None)

let apply_or_catch ?(fallback=fun _ _ -> ()) f x =
 try Some (f x) with e -> ((fallback e x); None)

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

(** {b Examples}: 
{[ 
# sprintf "[%4.2f]" None ;;  
  : string = "None"

# sprintf ~none:"NULL" "[%4.2f]" None ;;  
  : string = "NULL"

# Option.sprintf "[%4.2f]" (Some 3.14159) ;;
  : string = "Some [3.14]"
  
# Option.sprintf ~frame:"(The result is %s)" "[%4.2f]" (Some 3.14159) ;;
  : string = "(The result is [3.14])"
]}*)
let sprintf ?(none="None") ?frame fmt = 
  function 
  | None   -> none 
  | Some x -> 
     (match frame with
     | None      -> Printf.sprintf "Some %s" (Printf.sprintf fmt x) 
     | Some fmt' -> Printf.sprintf fmt' (Printf.sprintf fmt x) 
     )

let printf ?none ?frame fmt x = 
  Printf.printf "%s" (sprintf ?none ?frame fmt x)

let eprintf ?none ?frame fmt x = 
  Printf.eprintf "%s" (sprintf ?none ?frame fmt x)
     
let to_string ?none ?frame ?(a=fun _ -> "<abstr>") x =
  let y = map a x in
  sprintf ?none ?frame "%s" y

  