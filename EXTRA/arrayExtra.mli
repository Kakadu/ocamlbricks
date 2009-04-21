(** Additional features for the standard module [Array].*)

val of_known_length_list : ?reversing:bool -> int -> 'a list -> 'a array

val for_all : (int -> 'a -> bool) -> 'a array -> bool
val exists  : (int -> 'a -> bool) -> 'a array -> bool
val lexists : (int -> 'a -> bool) -> 'a array -> int option
val rexists : (int -> 'a -> bool) -> 'a array -> int option
