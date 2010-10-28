(* ----------------------------------------------------------
Compilation:
$ ocamlc -c -pp camlp4of -I +camlp4 camlp4of.cma log_module_loading_p4.ml

Usage:
$ ocamlc -c -pp "camlp4of log_module_loading_p4.cmo" your_source.ml
------------------------------------------------------------- *)
module type Unit = sig end
open Camlp4

(** Module registering the filter at loading-time as a side-effect. *)
module Log_module_loading_p4 : Unit = struct

 module Id = struct
  let name    = "log_module_loading_p4"
  let version = Sys.ocaml_version
 end

 module Make (AstFilters : Camlp4.Sig.AstFilters) = struct
  open AstFilters
  open Ast

  let first_str_item = ref true ;;
  (* let () = Printf.kfprintf flush stderr "camlp4: Registering filter log_module_loading_p4\n" ;; *)

  register_str_item_filter
   (Ast.map_str_item
    (function
     | s when !first_str_item ->
         (first_str_item := false);
         let loc = Ast.loc_of_str_item s in
         let file_name = Loc.file_name loc in
         (* Avoid circular recursion for Log and Meta modules: *)
         if List.mem file_name ["log.ml"; "meta.ml"] then
           begin
            (* Printf.kfprintf flush stderr "camlp4: Skipping to apply filter log_module_loading_p4 to %s\n" file_name; *)
             s
           end
         else begin
           (* Printf.kfprintf flush stderr "camlp4: Applying filter log_module_loading_p4 to %s\n" file_name; *)
	   let preambule = <:str_item@loc<
	      let () = Log.printf "Loading module %s\n" $str:file_name$
	      >>
	   in
	   StSem (loc, preambule, s)
	   end

     | s -> s )
    )#str_item
   ;;

 end

 let module M = Camlp4.Register.AstFilter(Id)(Make) in ()

end (* Log_module_loading_p4 *)
