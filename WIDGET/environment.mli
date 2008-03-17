class ['a, 'b] env :
  unit ->
  object
    val table : ('a, 'b) Hashmap.t
    method add : 'a * 'b -> unit
    method get : 'a -> 'b
    method get_l : ('a * 'b) list
    method set_l : ('a * 'b) list -> unit
    method updatedBy : ('a, 'b) env -> ('a, 'b) env
  end
val mkenv : ('a * 'b) list -> ('a, 'b) env
