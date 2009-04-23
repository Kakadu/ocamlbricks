(** Environments are set of bindings especially useful to express the result of a GUI dialog. *)

class ['a, 'b] env :
  unit ->
  object
    val table : ('a, 'b) Hashmap.t
    method add : 'a * 'b -> unit
    method add_list : ('a * 'b) list -> unit
    method get : 'a -> 'b
    method to_list : ('a * 'b) list
    method updatedBy : ('a, 'b) env -> ('a, 'b) env
  end
val make : ('a * 'b) list -> ('a, 'b) env

exception Undefined_identifier of string

class ['a] string_env :
  unit ->
  object
    val table : (string, 'a) Hashmap.t
    method add : string * 'a -> unit
    method add_list : (string * 'a) list -> unit
    method get : string -> 'a
    method to_list : (string * 'a) list
    method updatedBy : (string, 'a) env -> (string, 'a) env
  end

val make_string_env : (string * 'a) list -> 'a string_env
