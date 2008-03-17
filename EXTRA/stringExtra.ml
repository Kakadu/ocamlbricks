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


open ListExtra;;

(** Extra definitions. *)
module Extra = struct

(** Split a string into a list of strings containing 
    each one [n] characters of the input string (by default [n=1]). 
    {b Examples}:

{[# cut "aabbc";;
  : string list = ["a"; "a"; "b"; "b"; "c"]

# cut ~n:2 "aabbc";;
  : string list = ["aa"; "bb"; "c"]

# cut ~n:3 "aabbc";;
  : string list = ["aab"; "bc"]
]}*)
let cut ?(n:int=1) (s:string) = 
 let l = String.length s in
 let rec loop s l = 
  if l=0 then []  else
  if l<n then [s] else
   let l' = (l-n) in
   (String.sub s 0 n)::(loop (String.sub s n l') l')
 in loop s l
;; 

(** Similar to [cut ~n:1] but returns the list of {e characters} (instead of strings)
    of the input string. {b Example}:
{[# to_charlist "aaabbc";;
  : char list = ['a'; 'a'; 'a'; 'b'; 'b'; 'c']]}*)
let to_charlist (s:string) = 
 let l = String.length s in
 let rec loop s l = 
  if l=0 then []  else
   let l' = (l-1) in
   (String.get s 0)::(loop (String.sub s 1 l') l')
 in loop s l
;; 

 let rec split_old ?(d:char=' ') (s:string) = try
  let l = String.length s in
  let p = String.index s d in
   (StringLabels.sub ~pos:0 ~len:p s)::(split_old ~d (StringLabels.sub ~pos:(p+1) ~len:(l-p-1) s))
  with
   _ -> if (s="") then [] else [s]
;;

 
(** Split a string into a list of strings using a char delimiter (space (blank) by default).
    By default [squeeze=true], which means that delimiter repetitions are considered 
    as single occurrences.
    The empty string is converted into the empty list. {b Example}:
{[# split "aaa bbb ccc";;
  : string list = ["aaa"; "bbb"; "ccc"]

# split "aaa   bbb ccc";;
  : string list = ["aaa"; "bbb"; "ccc"]

# split ~squeeze:false "aaa   bbb ccc";;
  : string list = ["aaa"; ""; ""; "bbb"; "ccc"]
]}*)
let rec split ?(squeeze=true) ?(d:char=' ') (s:string) = try
  let l = String.length s in
  let p = String.index s d in
  let rest = split ~squeeze ~d (StringLabels.sub ~pos:(p+1) ~len:(l-p-1) s) in
  if squeeze && (p=0) then rest else (StringLabels.sub ~pos:0 ~len:p s)::rest
  with
   _ -> if (s="") then [] else [s]
;;

(** Merge two strings with a string separator. The call [merge sep x y] is simply equivalent to [x^sep^y].
    However, the partial application [merge sep] may be useful for defining a string list
    folding (see the next section {b Folding} and the examples in the subsection {b Common foldings} ). *)
let merge (sep:string) : (string -> string -> string)  = (fun x y -> x^sep^y) ;;

(** Quote a string using a prefix [l] (by default [l="'"]) and a suffix [r] (by default [r="'"]). *)
let quote ?(l="'") ?(r="'") (x:string) = l^x^r;;

(** Assemble a string with a prefix and a suffix but only if it is {b not} empty, else
     return the empty string ignoring the given prefix and suffix. *)
let assemble prefix x suffix = if (x="") then "" else (prefix^x^suffix)
;;

(** Convert a list of chars in a string.
{[# of_charlist ['h';'e';'l';'l';'o'];;
  : string = "hello"
]}*)
let of_charlist = List.fold_left  (fun s c -> (s^(Char.escaped c))) "" ;;

(** Curryfied binary operation on strings. *)
type binop = string -> string -> string ;;

(** The {e folding} of string lists is simply a [List.fold_left] specialization:  

     - the first element is the {b head} of the string list 
     - the folding is performed on the {b tail} of the string list. 
     - the iterated binary operation belongs the type [string -> string -> string]
     - if the input list is empty, the result is the {b empty} string (no exception raised)

   This function is adequate for most common cases. Use the module [Big] when 
   maximum generality is requested. *)
let big (f:binop) (l:string list) : 'a = try List.big f l with Failure "big" -> "" ;;

(** [merge_map f l] maps the function [f] on the list [l] 
    then merge the result with the separator ([sep=" "] by default). *)
let merge_map ?(sep=" ") f l = big (merge sep) (List.map f l) ;; 


(** Examples of applications of [big] constructor 
    in conjonction with the [merge] function. *)
module Fold = struct 

 (** Merge a string list with the separator [" , "]. *)
 let commacat = big (merge " , ");;

 (** Merge a string list with the separator ["; "]. *)
 let semicolon = big (merge "; ");;

 (** Merge a string list with the separator [","]. *)
 let nospacecommacat = big (merge ",");;

 (** Merge a string list with the separator [";"]. *)
 let nospacesemicolon = big (merge ";");;

 (** Merge a string list with the separator ["."]. *)
 let dotcat = big (merge ".");;

 (** Merge a string list with the separator ["\n"]. *)
 let newlinecat = big (merge "\n");;

 (** Merge a string list with the separator [" "]. *)
 let blankcat = big (merge " ");;

 (** Merge a string list with the separator ["/"]. *)
 let slashcat = big (merge "/");;

end;; (* module Fold *)

 (** Merge fields with a separator. {b Example}:
{[# merge_fields "/" [2;4] ["aaa";"bbb";"ccc";"ddd";"eee"] ;;
  : string = "ccc/eee"
]}*)
let rec merge_fields sep (fieldlist:int list) (l:string list) = 
 let l'=(List.select l fieldlist) in (big (merge sep) l')
;;


(** A {e line} is a string terminating with a newline ['\n']. *)
type line = string ;;

(** Convert a string in a {!line} just adding a newline {b if needed}. 
    The function {!chop} may be used as inverse.

{b Example}:
{[# to_line "hello";;
  : line = "hello\n"

# to_line "hello\n";;
  : line = "hello\n"]}*)
let to_line (x:string) : line = 
 let l    = (String.length x) in 
 let last = (String.sub x (l-1) 1) in 
 match last with "\n" -> x | _ -> x^"\n"  
;;


(** Converting raw text to list of strings and vice-versa. 
    A raw text is simply a (may be big) string, i.e. a sequence of lines 
    collected in a unique string, where each line terminates with a newline ['\n']. 
    The last line in the text may not terminate with a newline. *)
module Text = struct

(** A (line structured) text is a {b list} of strings. *)
type t = string list ;;

(** A text filter is a function from and to string lists. *)
type filter = string list -> string list ;;

(** Convert a string list in a raw text. 
    Each string in the input list is treated by the function {!to_line} in order to
    add a newline if needed, then the list is folded by a simple catenation ([^]).
    If the input list is empty, the result is the empty string. {b Examples}:
{[# Text.to_string ["AAA";"BBB";"CCC"];;
  : string = "AAA\nBBB\nCCC\n"

# Text.to_string ["AAA";"BBB\n";"CCC"];;
  : string = "AAA\nBBB\nCCC\n"

# Text.to_string ["AAA";"BBB\n\n";"CCC"];;
  : string = "AAA\nBBB\n\nCCC\n"]}*)
let to_string (sl : string list) : string = 
 let ll = List.map to_line sl in
 big (^) ll
;;


(** Convert a raw text in a structured text (a string list). 
    This function is simply an alias 
    for [split ~squeeze ~d:'\n']. {b Examples}:
{[# Text.of_string (Unix.cat "/etc/fstab")  ;;
  : string list =
["/dev/sda1   /                    reiserfs   acl,user_xattr    1 1";
 "/dev/sda3   swap                 swap       defaults          0 0";
 "/dev/sda4   /home                reiserfs   acl,user_xattr    1 1";
 "proc        /proc                proc       defaults          0 0";
 "/dev/fd0    /media/floppy        auto       noauto,user,sync  0 0"]

# Text.of_string (Unix.shell "echo aaa; echo; echo bbb");;
  : string list = ["aaa"; "bbb"]

# Text.of_string ~squeeze:false (Unix.shell "echo aaa; echo; echo bbb");;
  : string list = ["aaa"; ""; "bbb"] ]} *)
let of_string = (split ~d:'\n') ;;


(** Converting raw text to matrix (list of list) of strings (words) and vice-versa. *)
module Matrix = struct

(** A (word structured) text is a {b matrix} of strings. *)
type t = string list list;; 

(** A text matrix filter is a function from and to string list lists. *)
type filter = t -> t ;;

(** Convert a raw text in a matrix of words. 
    By default the word delimiter is the char [d=' ']
    and [squeeze=true]. 
{b Example}:
{[# Text.Matrix.of_string (Unix.shell "ls -i -w1 /etc/ssh/")  ;;
  : string list list =
[["98624"; "moduli"]; ["98625"; "ssh_config"]; ["98626"; "sshd_config"];
 ["274747"; "ssh_host_dsa_key"]; ["274748"; "ssh_host_dsa_key.pub"];
 ["274712"; "ssh_host_key"]; ["274713"; "ssh_host_key.pub"];
 ["274750"; "ssh_host_rsa_key"]; ["274751"; "ssh_host_rsa_key.pub"]]
]} *)
let of_string ?(squeeze=true) ?(d=' ') x = 
 List.map (split ~squeeze ~d) (of_string x) 
;; 

(** Convert a matrix of words in a raw text. 
    By default the word delimiter is the string [d=" "].
{[# let m = Text.Matrix.of_string (Unix.shell "ls -l /etc/ssh/") 
  in print_string (Text.Matrix.to_string m);;
total 164
-rw------- 1 root root 132839 2006-11-11 00:12 moduli
-rw-r--r-- 1 root root 2517 2006-11-11 00:12 ssh_config
-rw-r----- 1 root root 3474 2006-11-11 00:12 sshd_config
-rw------- 1 root root 668 2006-11-20 12:50 ssh_host_dsa_key
-rw-r--r-- 1 root root 600 2006-11-20 12:50 ssh_host_dsa_key.pub
-rw------- 1 root root 525 2006-11-20 12:50 ssh_host_key
-rw-r--r-- 1 root root 329 2006-11-20 12:50 ssh_host_key.pub
-rw------- 1 root root 887 2006-11-20 12:50 ssh_host_rsa_key
-rw-r--r-- 1 root root 220 2006-11-20 12:50 ssh_host_rsa_key.pub
  : unit = ()
]}*)
let to_string ?(d=" ") m = 
 to_line (big (merge "\n") (List.map (big (merge d)) m)) ;;

end;; (* module Text.Matrix *)


end;; (* module Text *)


 (** Remove from the input string the last chars in the set [['\n','\t',' ']]. 
     Similar to the [rstrip] {e Python} function.
     Example:
{[# chop "hell o \t\n";;
  : string = "hell o"]} *)
let rec chop x =  
  let l = (String.length x) in if (l=0) then x else 
   begin
   let last = (String.sub x (l-1) 1) in match last with
   | "\n" | " " | "\t" -> chop (String.sub x 0 (l-1))
   | _ -> x
   end
   ;;
(** Alias for {e Python} fans. *)        
let rstrip = chop;; 

(** As {!chop} but at left side. *)
let rec lstrip x =  
 let l = (String.length x) in if (l=0) then x else 
   begin
   let first = (String.sub x 0 1) in match first with
   | "\n" | " " | "\t" -> lstrip (String.sub x 1 (l-1))
   | _ -> x
   end
   ;;

(** As {!chop} but for both sides. *)
let strip x = lstrip (rstrip x)
;;


end;; (* module Extra *)


(** Redefinition of module [List]. *)
module String = struct 
 include String;;
 include Extra;;
end;;
