(* This file is part of our reusable OCaml BRICKS library
Copyright (C) 2008  Luca Saiu

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>. *)

open Ocamlbuild_plugin;;

let our_include_options = [A "-I"; A "+lablgtk2"];;
let our_byte_link_options = ["lablgtk.cma"];;    (* To do: actually_use_this *)
let our_native_link_options = ["lablgtk.cmxa"];; (* To do: actually_use_this *)

dispatch (function After_rules ->
  flag ["ocaml"; "compile"; "ourincludesettings"] (S our_include_options);
  flag ["ocaml"; "doc"; "ourocamldocsettings"]
       (S ([A "-keep-code"; A "-colorize-code"] @ our_include_options));
  | _ -> ());;

(*
dispatch begin function
  | After_rules ->
    ocaml_lib ~extern:true ~dir:"+lablgtk2" "lablgtk";
    flag
      ["ocaml"; "doc"; "ourocamldocsettings"]
      (S[A "-keep-code";
         A "-colorize-code";
         A "-I";
         A "+lablgtk2";
       ]);
| _ -> ()
end;;
*)

