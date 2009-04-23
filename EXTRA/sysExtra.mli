(** Additional features for the standard module [Sys]. *)

(** {2 Reading directories } *)

val readdir_into_list : ?namefilter:(string -> bool) -> ?nameconverter:(string -> string) -> string -> string list

(** {2 Rewriting files } *)

val put : ?callback:(string -> unit) -> string -> string -> unit

(** {2 Signals} *)

val int_of_signal    : int -> int
val string_of_signal : int -> string
