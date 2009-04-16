type 'a t

val create     : unit -> 'a t
val wait       : 'a t -> 'a
val release    : 'a t -> 'a -> unit
val status     : 'a t -> 'a option
