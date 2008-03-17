val default_size : int
class ['a, 'b] hashmultimap :
  ?size:int ->
  unit ->
  object
    val current : ('a, 'b) Hashtbl.t
    method add : 'a -> 'b -> unit
    method add_alist : ('a * 'b) list -> unit
    method bound : 'a -> bool
    method get : ('a, 'b) Hashtbl.t
    method lookup : 'a -> 'b list
    method lookup_or_fail : 'a -> 'b list
    method mem : 'a -> 'b -> bool
    method memq : 'a -> 'b -> bool
    method remove : ?all:bool -> 'a -> unit
    method replace : 'a -> 'b -> unit
    method to_alist : ('a * 'b) list
  end
type ('a, 'b) t = ('a, 'b) hashmultimap
val make : ?size:int -> unit -> ('a, 'b) t
val lookup_or_fail : ('a, 'b) t -> 'a -> 'b list
val lookup : ('a, 'b) t -> 'a -> 'b list
val mem : ('a, 'b) t -> 'a -> 'b -> bool
val memq : ('a, 'b) t -> 'a -> 'b -> bool
val bound : ('a, 'b) t -> 'a -> bool
val add : ('a, 'b) t -> 'a -> 'b -> unit
val add_alist : ('a, 'b) t -> ('a * 'b) list -> unit
val replace : ('a, 'b) t -> 'a -> 'b -> unit
val remove : ('a, 'b) t -> ?all:bool -> 'a -> unit
val update : ?replace:bool -> ('a, 'b) t -> ('a, 'b) t -> unit
val to_alist : ('a, 'b) t -> ('a * 'b) list
val from_alist : ?size:int -> ('a * 'b) list -> ('a, 'b) t
