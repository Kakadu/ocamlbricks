(* This file is part of our reusable OCaml BRICKS library
   Copyright (C) 2007-2009  Jean-Vincent Loddo

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

(** Additional features for the standard module [String].

{b Usage}:
-    {[ open StringExtra;; ]}
-    {[ module String = StringExtra.String;; ]}
The previous phrases are equivalent and allow you to access to additional features for strings.

You can give a look to the {!StringExtra.Extra} module documentation for more informations on these features.
*)


(** Extra definitions for strings. *)
module Extra = struct

(** {2 Importing & copying} *)

(** The type of the standard [String.blit]. *)
type blit_function = string -> int -> string -> int -> int -> unit

(** Make a blit function that uses the argument [~(perform:char->int->unit)]
    to perform an action for any scanned character. The first
    argument of [perform] is the character that will be copied,
    the second argument is the index in the target string. {b Example}:
{[# let perform c i = Printf.eprintf "Copying character %c at position %d\n" c i in
    let s = from_file ~blit:(blitting ~perform) "/etc/fstab" in
    ... ]} *)
let blitting ~(perform:char->int->unit) : blit_function =
 fun s1 ofs1 s2 ofs2 len ->
  if len < 0 || ofs1 < 0 || ofs1 > String.length s1 - len
             || ofs2 < 0 || ofs2 > String.length s2 - len
  then invalid_arg "String.blitting" else
  let ofs1=ref ofs1 in
  let ofs2=ref ofs2 in
  for i=1 to len do
    let c = s1.[!ofs1] in
    let i = !ofs2 in
    (perform c i);
    s2.[i] <- c;
    incr ofs1;
    incr ofs2;
  done
;;

(** Import the content of the [Unix] file descriptor. The optional [?(blit=String.blit)] allows
    to perform some operations during the copy of characters (see the function {!StringExtra.Extra.blitting}). *)
let from_descr ?(blit:blit_function=String.blit) (fd:Unix.file_descr) : string =
 let q = Queue.create () in
 let buffer_size = 8192 in
 let buff = String.create buffer_size in
 let rec loop1 acc_n =
  begin
   let n = (Unix.read fd buff 0 buffer_size)    in
   if (n=0) then acc_n else ((Queue.push ((String.sub buff 0 n),n) q); loop1 (acc_n + n))
   end in
 let dst_size = loop1 0 in
 let dst = String.create dst_size in
 let rec loop2 dstoff = if dstoff>=dst_size then () else
  begin
  let (src,src_size) = Queue.take q in
  (blit src 0 dst dstoff src_size);
  loop2 (dstoff+src_size)
  end in
 (loop2 0);
 dst
;;

(** Similar to {!StringExtra.Extra.from_descr}) but the user provides the file name instead of the file descriptor. *)
let from_file ?(blit:blit_function=String.blit) (filename:string) : string =
 let fd = (Unix.openfile filename [Unix.O_RDONLY;Unix.O_RSYNC] 0o640) in
 let result = from_descr ~blit fd in
 (Unix.close fd);
 result
;;

(** Similar to {!StringExtra.Extra.from_descr}) but the user provides the [Pervasives.in_channel] instead of the file descriptor. *)
let from_channel ?(blit:blit_function=String.blit) in_channel : string =
 from_descr ~blit (Unix.descr_of_in_channel in_channel)
;;

(** Make a copy of a string performing an action for any scanned character. *)
let from_string ~(perform:char->int->unit) (src:string) : string =
 let len = String.length src in
 let dst = String.create len in
 let blit = blitting ~perform in
 (blit src 0 dst 0 len);
 dst
;;

(** {2 Searching indexes} *)

(** [nth_index_from s n c nth] return the index of the [nth]
    occurrence of the character [c] searching in [s] from the offset [n].
    Raise [Not_found] if there isn't a sufficient number of occurrences. {b Example}:
{[# nth_index_from "@123@567@" 0 '@' 2;;
  : int = 4 ]}*)
let rec nth_index_from =
 let rec lloop s offset c k =
   if k=0 then offset else (* degenere *)
   if k=1 then String.index_from s offset c else
   let offset' = String.index_from s offset c in
   lloop s (offset'+1) c (k-1) in
 fun s n c k -> if k<0 then nth_rindex_from s n c (-k)
                       else lloop s n c k

(** As [nth_index_from] but searching from the {e right} to the {e left} side. *)
and nth_rindex_from =
 let rec rloop s offset c k =
   if k=0 then offset else (* degenere *)
   if k=1 then String.rindex_from s offset c else
   let offset' = String.rindex_from s offset c in
   rloop s (offset'-1) c (k-1) in
 fun s n c k -> if k<0 then nth_index_from s n c (-k)
                       else rloop s n c k
;;

(** As [nth_index_from] but searching from the beginning of the string (offset [0]). *)
let nth_index s  = nth_index_from  s 0;;

(** As [nth_rindex_from] but searching from the end of the string. *)
let nth_rindex s = nth_rindex_from s ((String.length s)-1);;

(** Similar to the standard [List.for_all], considering a string as a list of characters. *)
let for_all p s =
 let l = String.length s in
 let rec loop i =
  if i>=l then true else
  p s.[i] && loop (i+1)
 in loop 0
;;

(** Similar to the standard [List.exists], considering a string as a list of characters. *)
let exists p s =
 let l = String.length s in
 let rec loop i =
  if i>=l then false else
  p s.[i] || loop (i+1)
 in loop 0
;;

(** As the function {!StringExtra.Extra.exists}, but provides the index that verifies the predicate. *)
let lexists p s =
 let l = String.length s in
 let rec loop i =
  if i>=l then None else
  if p s.[i] then (Some i) else loop (i+1)
 in loop 0
;;

(** As the function [lexists], but searching from the right side. *)
let rexists p s =
 let l = String.length s in
 let rec loop i =
  if i<0 then None else
  if p s.[i] then (Some i) else loop (i-1)
 in loop (l-1)
;;

(** {2 Extracting sub-strings} *)

(** [tail s i] return the substring from the index [i] (included) to the end of [s].
    Raise [Invalid_argument "tail"] if the index is out of the string bounds. {b Example}:
{[# tail "azerty" 2;;
 : string = "erty" ]} *)
let tail s i =
 try String.sub s i ((String.length s)-i)
 with Invalid_argument _ -> raise (Invalid_argument "tail")
;;


(** [head s i] return the substring from the beginning of [s] to the index [i] included.
    Raise [Invalid_argument "head"] if the index is out of the string bounds. {b Example}:
{[# head "azerty" 2;;
 : string = "aze"
# head "azerty" 0 ;;
 : string = "a" ]} *)
let head s i =
 try String.sub s 0 (i+1)
 with Invalid_argument _ -> raise (Invalid_argument "head")
;;

(** [frame s c nth1 nth2] return the substring of [s] delimited by
    the [nth1] and the [nth2] occurrence of the character [c].
    Raise [Not_found] if the number of occurrences is lesser than [nth1].
    Raise [Invalid_argument "frame"] if [nth1] is greater than [nth2]. {b Example}:
{[# frame "\@xxx\@yyy\@zzz\@" '@' 1 3 ;;
  : string = "\@xxx\@yyy\@" ]}*)
let frame s c nth1 nth2 =
 if nth2<nth1 then (raise (Invalid_argument "frame")) else
 if nth2=nth1 then String.sub s (nth_index s c nth1) 1 else
 let offset1 = nth_index s c nth1 in
 let offset2 =  try
   nth_index_from s (offset1+1) c (nth2-nth1)
 with Not_found -> (String.length s)-1 in
 String.sub s offset1 (offset2-offset1+1)
;;

(** As [frame] but raise [Not_found] also if the number of occurrences is lesser than [nth2]. *)
let frame_strict s c nth1 nth2 =
 if nth2<nth1 then (raise (Invalid_argument "frame")) else
 if nth2=nth1 then String.sub s (nth_index s c nth1) 1 else
 let offset1 = nth_index s c nth1 in
 let offset2 = nth_index_from s (offset1+1) c (nth2-nth1) in
 String.sub s offset1 (offset2-offset1+1)
;;

(** As [frame] by searching and counting the number of occurrences
    from the {e right} to the {e left} side of string. {b Example}:
{[# rframe "\@xxx\@yyy\@zzz\@" '@' 1 3 ;;
  : string = "\@yyy\@zzz\@" ]} *)
let rframe s c nth1 nth2 =
 if nth2<nth1 then (raise (Invalid_argument "frame")) else
 if nth2=nth1 then String.sub s (nth_rindex s c nth1) 1 else
 let offset1 = nth_rindex s c nth1 in
 let offset2 = try
   nth_rindex_from s (offset1-1) c (nth2-nth1)
 with Not_found -> 0 in
 String.sub s offset2 (offset1-offset2+1)
;;

(** As [rframe] but raise [Not_found] also if the number of occurrences is lesser than [nth2]. *)
let rframe_strict s c nth1 nth2 =
 if nth2<nth1 then (raise (Invalid_argument "frame")) else
 if nth2=nth1 then String.sub s (nth_rindex s c nth1) 1 else
 let offset1 = nth_rindex s c nth1 in
 let offset2 = nth_rindex_from s (offset1-1) c (nth2-nth1) in
 String.sub s offset2 (offset1-offset2+1)
;;

(** {2 Counting} *)

(** Count the number of occurrences of the character in the string. *)
let count =
 let rec loop s c i acc =
  try let i = (String.index_from s i c) in loop s c (i+1) (acc+1)
  with Not_found -> acc
 in fun s c -> loop s c 0 0
;;

(** Note that the last index is (-1) when the character is not found. *)
let count_and_last_index =
 let rec loop s c i acc last_index =
  try let i = (String.index_from s i c) in loop s c (i+1) (acc+1) i
  with Not_found -> (acc,last_index)
 in fun s c -> loop s c 0 0 (-1)
;;

(** Note that the last two indexes may be (-1) if there isn't a sufficient number of occurrences. *)
let count_and_last_two_indexes =
 let rec loop s c i acc last_index penultimate =
  try let i = (String.index_from s i c) in loop s c (i+1) (acc+1) i last_index
  with Not_found -> (acc,last_index,penultimate)
 in fun s c -> loop s c 0 0 (-1) (-1)
;;

(** {2 Stripping} *)

(** [not_blank] stands for not [' '], not ['\t'] and not ['\n'] *)
let not_blank = (fun c -> (c<>' ') && (c<>'\t') && (c<>'\n'))
;;

(** Strip the left side of the string with the predicate {!StringExtra.Extra.not_blank} *)
let lstrip s =
 match lexists not_blank s with
 | None   -> ""
 | Some i -> String.sub s i (((String.length s))-i)
;;

(** Strip the right side of the string with the predicate {!StringExtra.Extra.not_blank} *)
let rstrip s =
 match rexists not_blank s with
 | None   -> ""
 | Some i -> String.sub s 0 (i+1)
;;

(** Strip the both sides of the string with the predicate {!StringExtra.Extra.not_blank} *)
let strip s =
 match (lexists not_blank s) with
 |  None   -> ""
 |  Some i -> (match (rexists not_blank s) with
	       | Some j -> String.sub s i (j-i+1)
               | None   -> assert false
               )
;;

(** Remove from the input string the last chars in the set [['\n','\t',' ']].
     Similar to the [rstrip] {e Python} function. Example:
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

(** {2 Splitting to char list} *)

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

(** Convert a list of chars in a string.
{[# of_charlist ['h';'e';'l';'l';'o'];;
  : string = "hello"
]}*)
let of_charlist = List.fold_left  (fun s c -> (s^(Char.escaped c))) "" ;;

(** More efficient char list operations (assemble/disassemble). *)
module Charlist = struct 

(** Fold a char list into a string. *)
let assemble (xs:char list) : string =
 let n = List.length xs in
 let s = String.make n ' ' in
 let rec loop i = function
  | []    -> ()
  | x::xs -> (String.set s i x); loop (i+1) xs
 in (loop 0 xs); s
;;

(** Disassemble (split) the string and return the reversed list of its characters. {b Example}:
{[# disassemble_reversing "abcd" ;;
 : char list = ['d'; 'c'; 'b'; 'a'] ]} *)
let disassemble_reversing ?(acc=[]) (s:string) : char list =
 let n = String.length s in
 let rec loop acc i = 
  if i>=n then acc else
  loop ((String.get s i)::acc) (i+1)
 in loop acc 0 
;;

(** Assemble a list of char into a string reversing the order. {b Example}:
{[# assemble_reversing ['a';'b';'c';'d'] ;;
 : string = "dcba" ]} *)
let assemble_reversing (xs:char list) : string =
 let n = List.length xs in
 let s = String.make n ' ' in
 let rec loop i = function
  | []    -> ()
  | x::xs -> (String.set s i x); loop (i-1) xs
 in (loop (n-1) xs); s
;;

end;;

(** {2 Splitting to string list} *)

(** Split a string into a list of strings containing 
    each one [n] characters of the input string (by default [n=1]). {b Examples}:
{[# cut "aabbc";;
  : string list = ["a"; "a"; "b"; "b"; "c"]

# cut ~n:2 "aabbc";;
  : string list = ["aa"; "bb"; "c"]

# cut ~n:3 "aabbc";;
  : string list = ["aab"; "bc"]
]} *)
let cut ?(n:int=1) (s:string) = 
 let l = String.length s in
 let rec loop s l = 
  if l=0 then []  else
  if l<n then [s] else
   let l' = (l-n) in
   (String.sub s 0 n)::(loop (String.sub s n l') l')
 in loop s l
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

(** Split a string into a string list using a list of blanks as word separators.
    Blanks are squeezed. {i Efficient version}.*)
let split_squeezing_blanks ?(blanks=['\t';' ']) (s:string) : string list =
 let xs = Charlist.disassemble_reversing s in
 let push_if_not_empty x l = if x=[] then l else (x::l) in
 let rec loop previous_blank acc1 acc2 = function
  | []      -> (push_if_not_empty acc1 acc2)
  | b ::xs when (List.mem b blanks) ->
	if previous_blank
	then loop true acc1 acc2 xs
	else loop true [] (push_if_not_empty acc1 acc2) xs
   |  x  ::xs -> loop false (x::acc1) acc2 xs
  in
  let xs = List.map Charlist.assemble (loop false [] [] xs) in
 xs
;;

(** {2 Merging strings} *)

(** Catenate a list of strings in an efficient way: the target string is created once
    (not as happen with a fold of [^]). The optional [?(blit=String.blit)] allows
    to perform some operations during the copy of characters (see the function {!StringExtra.Extra.blitting}). *)
let concat ?(blit:blit_function=String.blit) xs =
 let len  = List.fold_left (fun k s -> k+(String.length s)) 0 xs in
 let dst  = String.create len in
 let _ =
    List.fold_left
    (fun k src -> let l=(String.length src) in (blit src 0 dst k l); (k+l)) 0 xs in
 dst
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

(** Curryfied binary operation on strings. *)
type binop = string -> string -> string ;;

(** The {e folding} of string lists is simply a [List.fold_left] specialization:  

     - the first element is the {b head} of the string list 
     - the folding is performed on the {b tail} of the string list. 
     - the iterated binary operation belongs the type [string -> string -> string]
     - if the input list is empty, the result is the {b empty} string (no exception raised)

   This function is adequate for most common cases. Use the module [Big] when 
   maximum generality is requested. *)
let big (f:binop) (l:string list) : 'a = try ListExtra.big f l with Failure "big" -> "" ;;

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
 let l'=(ListExtra.select l fieldlist) in (big (merge sep) l')
;;

(** {2 Text} *)

(** A {e line} is a string terminating with a newline ['\n']. *)
type line = string ;;

(** Convert a string in a [line] just adding a newline {b if needed}.
    The function {!StringExtra.Extra.chop} may be used as inverse.

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
    Each string in the input list is treated by the function [to_line] in order to
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
end;; (* module Extra *)


(** Redefinition of the standard [String]. *)
module String = struct 
 include String;;
 include Extra;;
end;;
