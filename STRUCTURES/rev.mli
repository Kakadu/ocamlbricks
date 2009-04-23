(** Reversible references. The operation [open_parenthesis] set a backtracking point:
    all settings to reversible references could be reset by a simple call to [close_parenthesis].
    The user can nest parenthesis as he wishes. {b Example}:
{[# let x = create 42 ;;
val x : int t = {previous = []; current = 42}

# set x 43 ;;
  : unit = ()

# open_parenthesis () ;;
  : unit = ()

# set x 44 ;;
  : unit = ()

# set x 45 ;;
  : unit = ()

# get x;;
  : int = 45

# close_parenthesis () ;;
  : unit = ()

# get x;;
  : int = 43

# back_parenthesis () ;;
  : unit = ()

# get x;;
  : int = 42 ]} *)

type 'a t
val create : 'a -> 'a t

val open_parenthesis  : unit -> unit
val close_parenthesis : unit -> unit
val back_parenthesis  : unit -> unit

val get : 'a t -> 'a
val set : 'a t -> 'a -> unit

(** {2 Toolkit} *)

module Toolkit : sig
 val ref  : 'a   -> 'a t
 val (!)  : 'a t -> 'a
 val (:=) : 'a t -> 'a -> unit
end
