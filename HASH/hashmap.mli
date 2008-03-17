val default_size : int
class ['a, 'b] hashmap :
  ?size:int ->
  unit ->
  object
    val current : ('a, 'b) Hashtbl.t
    method add : 'a -> 'b -> unit
    method add_alist : ('a * 'b) list -> unit
    method bound : 'a -> bool
    method get : ('a, 'b) Hashtbl.t
    method lookup : 'a -> 'b
    method mem : 'a -> 'b -> bool
    method memq : 'a -> 'b -> bool
    method remove : 'a -> unit
    method replace : 'a -> 'b -> unit
    method to_alist : ('a * 'b) list
  end
type ('a, 'b) t = ('a, 'b) hashmap
val make : ?size:int -> unit -> ('a, 'b) t
val lookup : ('a, 'b) t -> 'a -> 'b
val mem : ('a, 'b) t -> 'a -> 'b -> bool
val memq : ('a, 'b) t -> 'a -> 'b -> bool
val bound : ('a, 'b) t -> 'a -> bool
val add : ('a, 'b) t -> 'a -> 'b -> unit
val add_alist : ('a, 'b) t -> ('a * 'b) list -> unit
val replace : ('a, 'b) t -> 'a -> 'b -> unit
val remove : ('a, 'b) t -> 'a -> unit
val update : ('a, 'b) t -> ('a, 'b) t -> unit
val to_alist : ('a, 'b) t -> ('a * 'b) list
val from_alist : ?size:int -> ('a * 'b) list -> ('a, 'b) t
