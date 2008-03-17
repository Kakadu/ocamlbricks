(** Additional features for the standard module [String]. 
    Open this module in order to use the extended version of [String] instead of
    the standard one. *)


module Extra :
  sig
   
   (** {2 Splitting} *)

    val cut : ?n:int -> string -> string list
    val to_charlist : string -> char list
    val split : ?squeeze:bool -> ?d:char -> string -> string list

   (** {2 Merging} *)

    val merge : string -> string -> string -> string
    val quote : ?l:string -> ?r:string -> string -> string
    val assemble : string -> string -> string -> string
    val of_charlist: char list -> string

   (** {2 Folding} *)

    type binop = string -> string -> string
    val big : binop -> string list -> string
    val merge_map : ?sep:string -> ('a->string) -> ('a list) -> string

   (** {3 Common foldings} *)

    module Fold : 
    sig
    val commacat : string list -> string
    val semicolon : string list -> string
    val nospacecommacat : string list -> string
    val nospacesemicolon : string list -> string
    val dotcat : string list -> string
    val newlinecat : string list -> string
    val blankcat : string list -> string
    val slashcat : string list -> string
    end

    (** {2 Records} *)
    (** String lists are considered in this section as 
        records of (string) fields. *)

    val merge_fields : string -> int list -> string list -> string

    (** {2 Text} *)
    (** Functions for manipulating texts, typically the output of a unix command. *)

   type line = string
   val to_line : string -> line

    module Text : 
     sig
        type t = string list
        type filter = t -> t ;;
        val to_string : string list -> string
        val of_string : ?squeeze:bool -> string -> string list
        module Matrix :
          sig
            type t = string list list
            type filter = t -> t ;;
            val of_string :
              ?squeeze:bool -> ?d:char -> string -> string list list
            val to_string : ?d:string -> string list list -> string
          end
     end
    
    (** {2 Stripping} *)

    val chop : string -> string
    val rstrip : string -> string
    val lstrip : string -> string
    val strip : string -> string
  end


module String :
  sig
    external length : string -> int = "%string_length"
    external get : string -> int -> char = "%string_safe_get"
    external set : string -> int -> char -> unit = "%string_safe_set"
    external create : int -> string = "caml_create_string"
    val make : int -> char -> string
    val copy : string -> string
    val sub : string -> int -> int -> string
    val fill : string -> int -> int -> char -> unit
    val blit : string -> int -> string -> int -> int -> unit
    val concat : string -> string list -> string
    val iter : (char -> unit) -> string -> unit
    val escaped : string -> string
    val index : string -> char -> int
    val rindex : string -> char -> int
    val index_from : string -> int -> char -> int
    val rindex_from : string -> int -> char -> int
    val contains : string -> char -> bool
    val contains_from : string -> int -> char -> bool
    val rcontains_from : string -> int -> char -> bool
    val uppercase : string -> string
    val lowercase : string -> string
    val capitalize : string -> string
    val uncapitalize : string -> string
    type t = string
    val compare : t -> t -> int
    external unsafe_get : string -> int -> char = "%string_unsafe_get"
    external unsafe_set : string -> int -> char -> unit
      = "%string_unsafe_set"
    external unsafe_blit : string -> int -> string -> int -> int -> unit
      = "caml_blit_string" "noalloc"
    external unsafe_fill : string -> int -> int -> char -> unit
      = "caml_fill_string" "noalloc"
    val cut : ?n:int -> string -> string list
    val to_charlist : string -> char list
    val split : ?squeeze:bool -> ?d:char -> string -> string list
    val merge : string -> string -> string -> string
    val quote : ?l:string -> ?r:string -> string -> string
    val assemble : string -> string -> string -> string
    val of_charlist: char list -> string

    type binop = string -> string -> string

    val big : binop -> string list -> string
    val merge_map : ?sep:string -> ('a->string) -> ('a list) -> string

    module Fold : 
    sig
    val commacat : string list -> string
    val semicolon : string list -> string
    val nospacecommacat : string list -> string
    val nospacesemicolon : string list -> string
    val dotcat : string list -> string
    val newlinecat : string list -> string
    val blankcat : string list -> string
    val slashcat : string list -> string
    end

    val merge_fields : string -> int list -> string list -> string

    type line = string
    val to_line : string -> line

    module Text :
      sig
        type t = string list
        type filter = t -> t ;;

        val to_string : string list -> string
        val of_string : ?squeeze:bool -> string -> string list
        module Matrix :
          sig
            type t = string list list
            type filter = t -> t ;;
            val of_string :
              ?squeeze:bool -> ?d:char -> string -> string list list
            val to_string : ?d:string -> string list list -> string
          end
      end

    val chop : string -> string
    val rstrip : string -> string
    val lstrip : string -> string
    val strip : string -> string
  end
