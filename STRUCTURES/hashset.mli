(** Polymorphic {e unbounded} sets.
    An encapsulated [('a, unit) Hashtbl.t] is used for quickly answering
    to the membership problem.  *)

type 'a t

val make    : ?size:int -> unit -> 'a t
val mem     : 'a t -> 'a -> bool
val add     : 'a t -> 'a -> unit
val remove  : 'a t -> 'a -> unit
val of_list : 'a list -> 'a t
val uniq    : 'a list -> 'a list

(** {2 Object-oriented interface} *)

class ['a] hashset :
  ?size:int ->
  unit ->
  object
    method add : 'a -> unit
    method mem : 'a -> bool
    method remove : 'a -> unit
  end
