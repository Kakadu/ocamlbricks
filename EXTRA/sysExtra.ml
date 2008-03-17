(* This file is part of our reusable OCaml BRICKS library
   Copyright (C) 2007  Jean-Vincent Loddo, Luca Saiu

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

(** Additional features for the standard module [Sys]. 
    Open this module in order to use the extended version of [Sys] instead of
    the standard one. *)

(** Extra definitions. *)
module Extra = struct

 (** {2 Reading directories } *)
  
 (** Reads a given directory, thus select and convert names. 
     @return the list of formatted names *) 
 let readdir_into_list ?(namefilter:(string->bool)=(fun x -> true)) ?(nameconverter:(string->string)=(fun x->x)) (dir:string) =
  try 
    let filelist  = (Array.to_list (Sys.readdir dir)) in
    let filter    = (fun n -> (try (namefilter n) with _ -> false)) in
    let selection = (List.filter filter filelist) in
    (List.map nameconverter selection)
  with _ -> [] 
 ;;



end;; (* module Extra *)


(** Redefinition of module [Sys]. *)
module Sys = struct
  include Sys;;
  include Extra;;
end;;
