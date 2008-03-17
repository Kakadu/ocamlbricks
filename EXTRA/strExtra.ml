(* This file is part of our reusable OCaml BRICKS library
   Copyright (C) 2007  Jean-Vincent Loddo

   Trivial changes:
   Copyright (C) 2007  Luca Saiu
   Other minor changes in 2008 by Luca Saiu

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

(** Additional features for the standard library [Str]. 
    Open this module in order to use the extended version of [Str] instead of
    the standard one. *)

open StringExtra;;

(** Extra definitions. *)
module Extra = struct

 (** The result of a matching of a regular expression with a string may be:
    
    [None] which means that the matching have failed 

    [Some(a,x,gl,b)] which means that:

	- there exists a substring [x] of the input string 
	  that matches the regular expression;
            
    	- the integers [a] and [b] (the {e frame}) are the positions (indexes) 
	  of the beginning and the end of the substring [x] w.r.t. the input string;

    	- the value [gl] is the list of substrings which have matched the groups defined in 
    	  the regular expression; the length of this list will be equal to the number of groups
    	  defined in the regular expression.

{b Example}:

{[# let r = mkregexp ["("]  ["[0-9]*"; "[,]?"; "[0-9]*"]  [")"] ;;

# match_whole r "abcd";;
  : result = None

# match_whole r "(16,7)";;
  : result = Some (0, "(16,7)", ["16"; ","; "7"], 5)

]}*)
 type result = (int * string * (string list) * int) option
 ;;

(** {2 Building} *)

(** Facility for building regular expressions. 
    The call [mkregexp pl gl sl] causes the following actions: 
    - the strings in [pl] are simply catenated in a unique string (the {e prefix}) 
    - the strings in [gl] are catenated enclosing each one into ["\\("] and ["\\)"] in order to define distinct {e groups}
    - the strings in [sl] are simply catenated in a unique string (the {e suffix})
    - the result is the compiled regexp of the catenation of {e prefix}, {e groups} and {e suffix}.
    The optional boolean parameter [strict], set to [true] by default, means that 
    the regular expression will be matched exactly. In other words, the string obtained
    as described above, by default is finally enclosed into ["^"] and ["$"].  
*)
let mkregexp ?(strict:bool=true) pl gl sl : Str.regexp = 

   let prefix = String.big (^) pl in
   let groups = String.big (^) (List.map (fun x->("\\(" ^ x ^ "\\)")) gl) in
   let suffix = String.big (^) sl in
   let expr = prefix ^ groups ^ suffix in
   let expr = if strict then ("^" ^ expr ^ "$") else expr in
   Str.regexp expr
;;

(** {2 Matching} *)

(** The call [matched_groups i x] returns the list 
    of substrings of [x] matching groups starting from the group number [i].
    See the standard [Str.matched_group] for more details. *)
let rec matched_groups i x : (string list) = 
 try
    let g=(Str.matched_group i x) in g::(matched_groups (i+1) x)
 with _ -> []
;;

(** The heuristic [match_frame r s (a,b)] try to match the substring [(a,b)] 
    of the string [s] with the compiled regular expression [r]. *)
 let match_frame (r:Str.regexp) (s:string) (a,b) : result =
  try begin 
  let s  = String.sub s a (b-a+1)    in  
  let i  = Str.search_forward r s 0 in
  let y  = Str.matched_string s      in
  let j  = (Str.match_end ())-1      in  Some (a+i,y,(matched_groups 1 s),a+j) 
  end with Not_found -> None
;;


(** The heuristic [match_whole r s (a,b)] try to match the whole string [s] 
    with the compiled regular expression [r]. *)
let match_whole (r:Str.regexp) (s:string) : result = 
 try
 let a  = Str.search_forward r s 0 in 
 let y  = Str.matched_string s      in
 let b  = (Str.match_end ())-1      in  Some (a,y,(matched_groups 1 s),b)
 with Not_found -> None
;;

(** Similar to [match_whole] but the regular expression is given as a simple string and compiled 
    on the fly before invoking [match_whole]. In other words, [match_string e s] is simpy 
    a shortcut for [match_whole (Str.regexp e) s]. *)
let match_string (expr:string) (s:string) : result = 
 match_whole (Str.regexp expr) s
;;

(** Extract parts of a string using a regexp containing some group expressions [\((..\))]. 
    If the input string does not match, the empty list is returned.
    {b Example}: 
{[# extract_groups (Str.regexp "aa\\([0-9]*\\)bb\\([A-Z]*\\)cc") "aa12bbZcc";;
  : string list = ["12"; "Z"]
]}*)
let extract_groups (r:Str.regexp) (s:string) : string list = 
   match match_whole r s with
   | None -> raise Not_found
(* To do: It was "[]". This may break things, and should be considered experimental --L. *)
   | Some (a,s,g,b) -> g
   ;; 


(** {3 Boolean versions} *)

(** Boolean versions of matching heuristics ([true] stands for [<>None]). *)
module Bool = struct 

  (** Boolean version of the heuristic [match_frame]. *)
  let match_frame  (r:Str.regexp) (s:string) (a,b) : bool = ((match_frame r s (a,b)) <> None)

  (** Boolean version of the heuristic [match_whole]. *)
  let match_whole  (r:Str.regexp) (s:string)       : bool = ((match_whole r s) <> None)

  (** Boolean version of the heuristic [match_string]. *)
  and match_string (e:string)     (s:string)       : bool = ((match_whole (Str.regexp e) s) <> None) 

end;; (* module Bool *)


(** {2 Stuff} *)


(** [minus x y] delete the {b rightmost} occurrence of the pattern [y] into the string [x].

    {b Examples}:
{[# minus "foo.bar.txt" "[.][a-z]*";;
  : string = "foo.bar"

# minus "/usr/local/bin" "[/][a-z]*";;
  : string = "/usr/local"

]}*)
let minus x y = 
 let pattern=("\\(.*\\)"^y^"\\(.*\\)") in 
 match (extract_groups (Str.regexp pattern) x) with 
 | [] -> x 
 | l  -> String.big (^) l 
;; 

(** Grep on string lists: only strings matching the pattern are selected.  

{b Examples}:
{[# grep "[0-9]" ["aa";"bb";"c8";"dd";"1e"]  ;;
  : string list = ["c8"; "1e"] 

# grep "[0-9]$" ["aa";"bb";"c8";"dd";"1e"]  ;;
  : string list = ["c8"]

# "ls" => ( Sys.run || fst || String.to_list || grep ".*mli$" ) ;; 
  : string list = ["foo.mli"; "bar.mli"] ]}
*)
let grep (e:string) (sl:string list) : string list = 
 let r = Str.regexp e in
 List.filter (Bool.match_whole r) sl
;;


(** Check if a string can be used as an identifier. *)
let wellFormedName ?(allow_dash=false) =
  if allow_dash then
    Bool.match_string "^[a-zA-Z][a-zA-Z0-9_\\-]*$"
  else
    Bool.match_string "^[a-zA-Z][a-zA-Z0-9_]*$";;

end;; (* module Extra *)


(** Redefinition of module [Str]. *)
module Str = struct

(** {2 Standard} *)

 include Str;;

(** {2 Extra} *)

(** For more details on additional definitions see the documentation 
    of the module {!module: StrExtra.Extra} *)
 include Extra;;

end;;
