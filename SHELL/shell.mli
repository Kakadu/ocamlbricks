type filename = string
type filexpr = string
type foldername = string
type line = string
type text = line list

val awk  : ?opt:string -> string -> text -> text
val cut  : string -> text -> text
val head : ?opt:string -> text -> text
val grep : ?opt:string -> string -> text -> text
val nl   : ?opt:string -> text -> text
val sed  : ?opt:string -> string -> text -> text
val sort : ?opt:string -> text -> text
val tac  : ?opt:string -> text -> text
val tail : ?opt:string -> text -> text
val tee  : ?opt:string -> filename list -> text -> text
val tr   : ?opt:string -> char -> char -> text -> text
val uniq : ?opt:string -> text -> text
val wc   : text -> int
val cc   : ?strict:bool -> text -> int

module Files :
  sig
    val glob : ?null:bool  -> filexpr -> text
    val cat  : ?opt:string -> filexpr -> text
    val cut  : ?opt:string -> filexpr -> text
    val head : ?opt:string -> filexpr -> text
    val nl   : ?opt:string -> filexpr -> text
    val sort : ?opt:string -> filexpr -> text
    val tac  : ?opt:string -> filexpr -> text
    val tail : ?opt:string -> filexpr -> text
    val uniq : ?opt:string -> filexpr -> text
  end

val date   : ?opt:string -> ?arg:string -> unit -> string
val id     : ?opt:string -> ?arg:string -> unit -> string
val uname  : ?opt:string -> unit -> string
val whoami : unit -> string
val find   : string -> text

val dd :
  ?ibs:int option ->
  ?obs:int option ->
  ?bs:int option ->
  ?cbs:int option ->
  ?skip:int option ->
  ?seek:int option ->
  ?count:int option -> ?conv:int option -> filename -> filename -> unit

val tgz_create : ?opt:string -> filename -> filexpr -> unit
val tgz_extract : ?opt:string -> filename -> foldername -> unit

module Check :
  sig
    val check : Wrapper.command -> string -> ?nullglob:bool -> filexpr -> bool
  end

val dir_writable       : ?nullglob:bool -> filexpr -> bool
val dir_comfortable    : ?nullglob:bool -> filexpr -> bool
val regfile_readable   : ?nullglob:bool -> filexpr -> bool
val regfile_modifiable : ?nullglob:bool -> filexpr -> bool
val freshname_possible : string -> bool
