(** Additional features for the standard module [Mutex].

{b Example}:
{[(* Extend the standard Mutex: *)
module Mutex = MutexExtra.Extend (Mutex);;

(* Use the function with_mutex: *)
Mutex.with_mutex mutex (fun () -> ...);; ]}
 *)

module Extend : functor

  (Mutex : sig
     type t
     val lock   : t -> unit
     val unlock : t -> unit
  end) ->

  sig
    val with_mutex : Mutex.t -> (unit -> 'a) -> 'a
  end
