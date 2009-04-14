module Source :
 sig
  type t =
  | Unix_descr of Unix.file_descr
  | In_channel of in_channel
  | Filename   of string
  | String     of string

  val to_file_descr : t -> Unix.file_descr * bool
  val to_string : t -> string
 end

module Sink :
 sig
  type t =
  | Unix_descr  of Unix.file_descr
  | Out_channel of out_channel
  | Filename    of string
  | Fun_thread  of (Unix.file_descr -> unit)

  val to_file_descr : t -> Unix.file_descr * bool
  val to_string : t -> string
 end
