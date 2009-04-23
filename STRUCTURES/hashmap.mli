(** Polymorphic {e unbounded} maps (environments). *)

type ('a, 'b) t

val make     : ?size:int -> unit -> ('a, 'b) t
val lookup   : ('a, 'b) t -> 'a -> 'b
val mem      : ('a, 'b) t -> 'a -> 'b -> bool
val memq     : ('a, 'b) t -> 'a -> 'b -> bool
val bound    : ('a, 'b) t -> 'a -> bool
val add      : ('a, 'b) t -> 'a -> 'b -> unit
val add_list : ('a, 'b) t -> ('a * 'b) list -> unit
val replace  : ('a, 'b) t -> 'a -> 'b -> unit
val remove   : ('a, 'b) t -> 'a -> unit
val update   : ('a, 'b) t -> ('a, 'b) t -> unit
val to_list  : ('a, 'b) t -> ('a * 'b) list
val of_list  : ?size:int -> ('a * 'b) list -> ('a, 'b) t

(** {2 Object-oriented interface} *)

class ['a, 'b] hashmap :
  ?size:int ->
  unit ->
  object
    method add      : 'a -> 'b -> unit
    method add_list : ('a * 'b) list -> unit
    method bound    : 'a -> bool
    method get      : ('a, 'b) Hashtbl.t
    method lookup   : 'a -> 'b
    method mem      : 'a -> 'b -> bool
    method memq     : 'a -> 'b -> bool
    method remove   : 'a -> unit
    method replace  : 'a -> 'b -> unit
    method to_list  : ('a * 'b) list
  end
