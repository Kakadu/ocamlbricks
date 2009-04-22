(** Additional features for the standard module [Array].*)

val of_known_length_list : ?reversing:bool -> int -> 'a list -> 'a array

val for_all : (int -> 'a -> bool) -> 'a array -> bool
val exists  : (int -> 'a -> bool) -> 'a array -> bool
val lexists : (int -> 'a -> bool) -> 'a array -> int option
val rexists : (int -> 'a -> bool) -> 'a array -> int option

module Matrix : sig
 type 'a t = 'a array array
 val init : int -> int -> (int -> int -> 'a) -> 'a t
 val of_list : 'a list list -> 'a t
 val to_list : 'a t -> 'a list list
 val transpose : 'a t -> 'a t
end
