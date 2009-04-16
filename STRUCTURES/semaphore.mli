type t
val create : ?mutex:Mutex.t -> ?condition:Condition.t -> ?init:int -> unit -> t

val p        : ?n:int -> t -> unit
val v        : ?n:int -> t -> unit
val p_nowait : ?n:int -> t -> bool

val with_semaphore : ?n:int -> t -> (unit -> 'a) -> 'a

module Array :
  functor (M : sig val dim : int end) ->
    sig

      val dim : int
      val create : ?mutex:Mutex.t -> ?condition:Condition.t -> ?init:int array -> unit -> t array

      val p        : ?n:int array -> t array -> unit
      val v        : ?n:int array -> t array -> unit
      val p_nowait : ?n:int array -> t array -> bool

      val with_semaphore : ?n:int array -> t array -> (unit -> 'a) -> 'a

    end
