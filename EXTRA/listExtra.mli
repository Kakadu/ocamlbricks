(** Additional features for the standard module [List]. *)

val foreach      : 'a list -> ('a -> unit) -> unit

(** {2 Generalizations} *)

val head : ?n:int -> 'a list -> 'a list
val tail : ?i:int -> 'a list -> 'a list

(** {2 Set operations} *)

val substract    : 'a list -> 'a list -> 'a list
val subset       : 'a list -> 'a list -> bool
val eqset        : 'a list -> 'a list -> bool
val intersection : 'a list -> 'a list -> 'a list
val uniq         : 'a list -> 'a list

(** {2 Indexes} *)

val range      : int -> int -> int list
val interval   : int -> int -> int list
val indexes    : 'a list -> int list
val asFunction : int list -> int -> int

(** {b Selecting by indexes} *)

val select : 'a list -> int list -> 'a list

(** {b Removing by indexes} *)

val rmindex : 'a list -> int -> 'a list

(** {b Searching for indexes} *)

val indexSuchThat : ('a -> bool) -> 'a list -> int option
val indexOf       : 'a -> 'a list -> int option
val firstIndexOf  : 'a -> 'a list -> int option
val lastIndexOf   : 'a -> 'a list -> int option

(** {2 Permutations} *)

val shuffle : 'a list -> 'a list
val permute : (int -> int) -> 'a list -> 'a list
val shuffler : 'a list -> int -> int
val shuffleIndexes : 'a list -> int list

(** {2 Folding} *)

val big : ('a -> 'a -> 'a) -> 'a list -> 'a
val max : 'a list -> 'a
val min : 'a list -> 'a

(** {2 List of lists} *)

val transpose : 'a list list -> 'a list list
