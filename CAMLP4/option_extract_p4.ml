(* ----------------------------------------------------------
Compilation:
$ ocamlc -c -pp camlp4of -I +camlp4 camlp4of.cma option_extract_p4.ml

Usage:
$ ocamlc -c -pp "camlp4of option_extract_p4.cmo" your_source.ml
------------------------------------------------------------- *)
module type Unit = sig end
open Camlp4

(* Module registering the filter at loading-time as a side-effect. *)
module Option_extract_traced : Unit = struct

 module Id = struct
  let name    = "option_extract_p4"
  let version = Sys.ocaml_version
 end

 module Make (AstFilters : Camlp4.Sig.AstFilters) = struct
  open AstFilters
  open Ast

  register_str_item_filter
   (Ast.map_expr
    (function
     | <:expr@loc< Option.extract >> ->
         let failwith_msg = Loc.to_string loc in
         <:expr@loc< Option.extract ~failwith_msg:$str:failwith_msg$ >>
     | e -> e )
    )#str_item

 end

 let module M = Camlp4.Register.AstFilter(Id)(Make) in ()

end (* Option_extract_traced *)
