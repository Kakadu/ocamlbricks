type ('a,'b) t

val make    : ?trace:bool -> ?size:int -> unit -> ('a,'b) t
val call    : ('a,'b) t -> ('a->'b) -> 'a -> 'b

