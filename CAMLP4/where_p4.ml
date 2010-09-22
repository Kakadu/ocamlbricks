(* ----------------------------------------------------------
Compilation:
$ ocamlc -c -pp camlp4of -I +camlp4 camlp4of.cma where_p4.ml

Usage:
$ ocamlc -c -pp "camlp4of where_p4.cmo" your_source.ml
------------------------------------------------------------- *)
module type Unit = sig end
open Camlp4

module Where_p4 : Unit = struct

 module Id = struct
  let name    = "where_p4"
  let version = Sys.ocaml_version
 end

 module Make (Syntax : Sig.Camlp4Syntax) = struct
  open Sig
  include Syntax

  EXTEND Gram
    GLOBAL: str_item sig_item expr binding;

    str_item: FIRST
      [[ s1 = str_item; [ "WHERE" | "where" ]; s2 = str_item -> Ast.StSem (_loc,s2,s1) ]];

    sig_item: FIRST
      [[ s1 = sig_item; [ "WHERE" | "where" ]; s2 = sig_item -> Ast.SgSem (_loc,s2,s1) ]];

    expr: FIRST
      [[ e = expr; [ "where" ]; b = binding -> <:expr< let $b$ in $e$ >> ]];

  END
 end (* Make *)

let module M = Register.OCamlSyntaxExtension(Id)(Make) in ()
end (* Where_p4 *)
