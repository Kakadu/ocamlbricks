(* ----------------------------------------------------------
Compilation:
$ ocamlc -c -pp camlp4of -I +camlp4 camlp4of.cma raise_p4.ml

Usage:
$ ocamlc -c -pp "camlp4of raise_p4.cmo" your_source.ml
------------------------------------------------------------- *)
module type Unit = sig end
open Camlp4

(* Module registering the filter at loading-time as a side-effect. *)
module Raise_p4 : Unit = struct

 module Id = struct
  let name    = "raise_p4"
  let version = Sys.ocaml_version
 end

 module Tool = struct
   INCLUDE "CAMLP4/common_tools_for_preprocessors.ml"
 end

 module Make (AstFilters : Camlp4.Sig.AstFilters) = struct
  open AstFilters
  open Ast

  let module M = Tool.Add_directive_syntax_extension (struct end) in ()
  ;;

  let escape_raise_filter = ref false ;;
  let verbosity = ref "1" ;;

  register_str_item_filter
   (Ast.map_str_item
    (function
     (* %str_item escape_raise_filter *)
     | <:str_item@loc< # escape_raise_filter >> ->
         (escape_raise_filter := true);
         <:str_item@loc< >>

     (* %str_item enable_raise_filter *)
     | <:str_item@loc< # enable_raise_filter >> ->
         (escape_raise_filter := false);
         <:str_item@loc< >>

     (* %str_item set_raise_filter_verbosity <verbosity> *)
     | <:str_item@loc< # set_raise_filter_verbosity $int:k$ >> ->
         (verbosity := k);
         <:str_item@loc< >>

     | s -> s )
    )#str_item
   ;;

  register_str_item_filter
   (Ast.map_expr
    (function

     | <:expr@loc< raise $e$ >> when (not !escape_raise_filter) ->
         let loc_as_string = Loc.to_string loc in
         <:expr@loc<
            let __raised_exception__ = $e$ in
            let () =
              Log.printf ~v:$int:!verbosity$ "Raising exception %s at %s\n"
                (Printexc.to_string __raised_exception__)
                 $str:loc_as_string$
            in
            raise __raised_exception__
         >>
     | e -> e )
    )#str_item

 end

 let module M = Camlp4.Register.AstFilter(Id)(Make) in ()

end (* raise_p4 *)
