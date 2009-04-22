(** Additional features for the standard library [Str]. *)

val mkregexp : ?strict:bool -> string list -> string list -> string list -> Str.regexp

(** {2 High-level matching} *)

type result = (int * string * string list * int) option

val match_whole    : Str.regexp -> string -> result
val match_string   : string -> string -> result
val match_frame    : Str.regexp -> string -> int * int -> result

(** {b Boolean versions} *)

module Bool :
  sig
    val match_whole : Str.regexp -> string -> bool
    val match_string : string -> string -> bool
    val match_frame : Str.regexp -> string -> int * int -> bool
  end

(** {2 Extract groups} *)

val matched_groups : int -> string -> string list
val extract_groups : Str.regexp -> string -> string list

(** {2 Tools} *)

val minus          : string -> string -> string
val grep           : string -> string list -> string list
val wellFormedName : ?allow_dash:bool -> string -> bool
