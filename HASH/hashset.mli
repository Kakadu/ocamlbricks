type 'a t

val make    : ?size:int -> unit -> 'a t
val of_list : 'a list    -> 'a t

val mem     : 'a t -> 'a -> bool
val add     : 'a t -> 'a -> unit
val remove  : 'a t -> 'a -> unit

val uniq    : 'a list -> 'a list

