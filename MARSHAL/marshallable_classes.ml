(*  This file is part of ocamlbricks
    Copyright (C) 2012  Jean-Vincent Loddo

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.*)

(* Do not remove the following comment: it's an ocamldoc workaround. *)
(** *)

let marshallable_classes_version = "0.1" ;;

let marshallable_classes_metadata () =
 Printf.sprintf "marshallable_classes version %s (executable %s)
Copyright (C) 2012  Jean-Vincent Loddo,
This is free software; see the source for copying conditions.
There is NO warranty; not even for MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.
" (marshallable_classes_version) (Sys.executable_name)
;;

(* ********************************* *
         WARNINGS and TRACING
 * ********************************* *)
let warnings = ref true ;;
let enable_warnings  () = (warnings := true);; 
let disable_warnings () = (warnings := false);; 
let are_warnings_disabled () = not !warnings

let tracing = ref false ;;
let enable_tracing  () = (tracing := true);; 
let disable_tracing () = (tracing := false);; 
let is_tracing_enable () = !tracing
let is_tracing_disabled () = not !tracing

(* ********************************* *
              M A I N
 * ********************************* *)

let with_open_in_bin ~filename mtdh =
  let in_channel = open_in_bin filename in
  let length = in_channel_length (in_channel) in
  let result = mtdh in_channel length in
  close_in in_channel;
  result
;;

let with_open_out_bin ?(perm=0o644) ~filename mtdh =
  let file_exists = Sys.file_exists filename in
  let out_channel = open_out_gen [Open_wronly; Open_creat; Open_trunc; Open_binary] 0 filename in
  let result = mtdh out_channel in
  close_out out_channel;
  if not file_exists then Unix.chmod filename perm else ();
  result
;;

let get_file_content ~filename =
  with_open_in_bin ~filename
    (fun in_channel length ->
       let s = String.create length in
       really_input in_channel s 0 length;
       s)
;;

let set_file_content ?perm ~filename content =
  with_open_out_bin ?perm ~filename
    (fun out_channel -> output_string out_channel content)
;;

module Option = struct
let extract =
 function
  | None   -> failwith "Option.extract"
  | Some x -> x

let map f = function None -> None | Some x -> Some (f x)
let iter f = function None -> () | Some x -> (f x)
let extract_or_force xo y = match xo with
 | Some x -> x
 | None   -> Lazy.force y
end

(* A reference-based map to map&fold converter: *)
let map_and_fold map (f:('a -> 'b -> 'c * 'a)) (s0:'a) xs =
  let state = ref s0 in
  let ys = map (fun x -> let (c,a) = f !state x in state := a; c) xs in
  (ys, !state)

 
type oid = Oid of int
type index = int
type next_index = int

type magic = int ref (* just a pointer... *)

type ('a,'b) either = Left of 'a | Right of 'b

module Extend_Map = functor (M:Map.S) -> struct
  include M

  let of_list (xs : (key * 'a) list) : 'a t =
    List.fold_left (fun m (k,a) -> add k a m) empty xs

  let to_list (m : 'a t) =
    fold (fun k a xs -> (k,a)::xs) m []
    
  let filter_mapi (p : key -> 'a -> bool) (f:key -> 'a -> 'b) (m : 'a t) : 'b t =
    fold (fun k a m' -> if p k a then add k (f k a) m' else m') m empty

  let product (m1:'a t) (m2:'b t) : ('a * 'b) t = 
    filter_mapi (fun k _ -> mem k m2) (fun k a -> (a, (find k m2))) m1  
  
  let length (m : 'a t) =
    fold (fun k a n -> n+1) m 0
    
end

(* Note that compare is a flipped version of the provided one in order
   to have the result of to_list automatically sorted by key (from the
   lesser to the greater key, in the sense of the provided compare). *)
module Map_Make (Ord : Map.OrderedType) = 
  Extend_Map 
    (Map.Make (struct include Ord let compare x y = compare y x end))

module Oid_map = Map_Make (struct type t = oid let compare = compare end)
module Int_map = Map_Make (struct type t = int let compare = compare end)
module String_map = Map_Make (struct type t = string let compare = compare end)

(* In any case marshallable without closures: *)  
type marshallable = 
  | Pointer of index (* to an object or a string (representing the object) *)
  | Datum of magic

(* Marshalling environments: *)
module Marshalling_env = struct
 type t = next_index * oid_index_env * index_string_env
  and oid_index_env    = index  Oid_map.t (* oid   -> index *)
  and index_string_env = string Int_map.t (* index -> string *)

 let initial ~parent_oid =
   let parent_index = 0 in
   let next_index = parent_index+1 in
   let oid_index_env = 
      Oid_map.add (Oid parent_oid) (parent_index) (Oid_map.empty)
   in 
   let index_string_env = Int_map.empty in
   (next_index, oid_index_env, index_string_env)

 let search_index_by_oid oid t =
   let (next_index, oid_index_env, index_string_env) = t in
   try 
     Some (Oid_map.find oid oid_index_env)
   with Not_found -> None 

 let add_oid_and_get_index (oid:oid) (t:t) : (t * index) =
   let (next_index, oid_index_env, index_string_env) = t in
   let index = next_index in
   let oid_index_env' =  Oid_map.add oid index oid_index_env in
   let next_index'    = index + 1 in
   let t' = (next_index', oid_index_env', index_string_env) in
   (t', index)
   
 let add_marshalled_object (index:index) (str:string) (t:t) : t =
   let (next_index, oid_index_env, index_string_env) = t in
   let index_string_env' = Int_map.add index str index_string_env in 
   let t' = (next_index, oid_index_env, index_string_env') in
   t'

 let extract_index_string_env (t:t) : (index * string) (* ordered *) list =
   let (next_index, oid_index_env, index_string_env) = t in
   Int_map.to_list index_string_env
   
end (* module Marshalling_env *)

(* Unmarshalling environments: *)
module Unmarshalling_env = struct

 (* index -> (Left of string | Right of object) *)
 type t = {
   index_map : ((string, magic) either) Int_map.t;
   label_mapping : (string -> string) option
   }
  
 let get_string_or_object_by_index index t =
   Int_map.find (index) t.index_map

 let replace_string_with_object (index:index) obj t =
   {t with index_map = Int_map.add index (Right obj) t.index_map; }

 let initial ?mapping ~(parent:magic) ~(index_string_list: (index * string) list) () =
   let parent_index = 0 in
   let index_string_env = Int_map.of_list index_string_list in
   let imported_index_string_env = 
     Int_map.map (fun str -> Left str) index_string_env 
   in
   let label_mapping = 
     Option.map 
       (fun f -> fun x -> try f x with _ -> x)
       mapping
   in   
   { index_map = Int_map.add parent_index (Right parent) (imported_index_string_env);
     label_mapping = label_mapping; }
   
 let extract_label_mapping t = t.label_mapping 
   
end (* module Marshalling_env *)

type field_name = string
type label = string
type labelled_values = (label * marshallable) list
type unlabelled_values = marshallable list

(* The ready-to-be-marshalled structure representing objects: *)
type mystic_structure = {
  labelled_values   : labelled_values;
  unlabelled_values : unlabelled_values;
  index_string_list : (index * string) (* ordered *) list;
  }

module Fields_register = struct

 (* Variables that I call "saa" are of this type: *)
 type string_adapted_accessors = {
   get : Marshalling_env.t   -> unit -> marshallable * Marshalling_env.t;
   set : Unmarshalling_env.t -> marshallable -> unit * Unmarshalling_env.t;
   }

 type t = {
   anonymous_fields : string_adapted_accessors list;         (* The order here is relevant *)
   named_fields     : string_adapted_accessors String_map.t; (* field-name -> string_adapted_accessors *)
   }
   
 let empty : t = { 
   anonymous_fields = [];
   named_fields = String_map.empty; 
   }
   
 let add ?field_name saa t =
   match field_name with
   | None      -> {t with anonymous_fields=(saa :: t.anonymous_fields)}
   | Some name -> {t with named_fields = String_map.add name saa t.named_fields}
   
 let match_named_fields_with_labelled_values ?class_name ?label_mapping (t:t) (labelled_values: (string * 'a) list) 
   : (string_adapted_accessors * 'a) list 
   =
   let labelled_values = match label_mapping with
   | None   -> labelled_values
   | Some f -> List.map (fun (k,v) -> ((f k),v)) labelled_values
   in
   let labelled_values = String_map.of_list labelled_values in
   let matching = String_map.product t.named_fields labelled_values in
   let matching_as_list = String_map.to_list matching in
   let () = 
     if are_warnings_disabled () then () else
     let nf = String_map.length t.named_fields in
     let nv = String_map.length labelled_values in
     let nm = List.length matching_as_list in
     let what = lazy (match class_name with
     | None -> "an object"
     | Some name -> Printf.sprintf "a `%s' instance" name)
     in
     match (compare nf nv), (nm < (min nf nv)) with
     | -1, false -> 
	Printf.kfprintf flush stderr
	  "Warning: loading %s from a serialized richer object (%d labelled values expected, %d found).\n" 
	  (Lazy.force what) nf nv
     |  1, false -> 
	Printf.kfprintf flush stderr
	  "Warning: loading %s from a serialized poorer object (%d labelled values expected, %d found).\n" 
	  (Lazy.force what) nf nv
     |  0, false -> ()
     | _ -> 
	Printf.kfprintf flush stderr
	  "Warning: loading %s from a serialized different object (%d common fields, %d unloaded fields, %d unused values).\n" 
	  (Lazy.force what) nm (nf-nm) (nv-nm)
   in
   (* Now forget field names: *)
   List.map snd matching_as_list

 let match_anonymous_fields_with_unlabelled_values ?class_name (t:t) (unlabelled_values: 'a list) 
   : (string_adapted_accessors * 'a) list 
   =
   let what = lazy (match class_name with
   | None -> "an object"
   | Some name -> Printf.sprintf "a `%s' instance" name)
   in
   let rec combine fs vs =
     match (fs, vs) with
     | ([],[]) -> []
     | (f::fs, v::vs) -> (f,v)::(combine fs vs)
     | ([],_) -> 
         let () = 
           if are_warnings_disabled () then () else
           Printf.kfprintf flush stderr
             "Warning: loading the anonymous fields of %s from a serialized richer object (%d values expected, %d found).\n"
             (Lazy.force what)
             (List.length t.anonymous_fields)
             (List.length unlabelled_values)
         in []
     | (_,[]) -> 
         let () = 
           if are_warnings_disabled () then () else
           Printf.kfprintf flush stderr
             "Warning: loading the anonymous fields of %s from a serialized poorer object (%d values expected, %d found).\n"
             (Lazy.force what)
             (List.length t.anonymous_fields)
             (List.length unlabelled_values)
         in []
   in
   combine t.anonymous_fields unlabelled_values
  
end (* module Fields_register *)  
  

class marshallable_class ?name ~(marshaller:marshaller option ref) () =
let shared_marshaller = marshaller in
object (self)

  (* When objects will be initialized, the shared_marshaller will be defined once by 
     the first inherited, that is precisely this class (marshallable_class): *)
  method marshaller : marshaller =
    match !shared_marshaller with Some x -> x | None -> assert false

  (* The first initializer wins: *)  
  initializer 
    match !shared_marshaller with
    | None   -> 
        begin
          let created_marshaller = 
            new marshaller ?parent_class_name:name ~parent:(self :> (marshallable_class)) ()
          in
          let () = if is_tracing_disabled () then () else
            Printf.kfprintf flush stderr 
              "%s.marshallable_class(%d).initializer: created the marshaller %d\n" 
              (Option.extract name) (Oo.id self) (Oo.id created_marshaller)
          in
          shared_marshaller := (Some created_marshaller); (* Release the information to the parent *)
        end
    | Some m -> 
          let () = if is_tracing_disabled () then () else
            Printf.kfprintf flush stderr 
              "%s.marshallable_class(%d).initializer: sharing the marshaller %d\n" 
              (Option.extract name) (Oo.id self) (Oo.id m)
          in
          () (* It's fine, a shared marshaller has been already created *)

end

and (* class *) marshaller ?parent_class_name ~(parent:marshallable_class) () =
let _WITHOUT_CLOSURES_OF_COURSE = [] in
object (self)

  val parent_class_name : string option = parent_class_name
  method parent_class_name = parent_class_name
  
  val mutable fields_register : Fields_register.t = 
    Fields_register.empty

  method private get_fields_register = fields_register
  
  method private get_anonymous_fields = 
    fields_register.Fields_register.anonymous_fields
  
  method private get_named_fields_as_ordered_assoc_list = 
    let m = fields_register.Fields_register.named_fields in
    String_map.to_list m

  method private register_string_adapted_accessors ?field_name (saa : Fields_register.string_adapted_accessors) =
     fields_register <- (Fields_register.add ?field_name saa fields_register)

  (* -----------------------------
       Registering SIMPLE fields
     ----------------------------- *)

  method private adapt_simple_field : 'a. (unit -> 'a) -> ('a -> unit) -> Fields_register.string_adapted_accessors =
     fun real_get real_set ->
     let string_adapted_get : Marshalling_env.t -> unit -> marshallable * Marshalling_env.t =
       fun marshalling_env () ->
         let x = real_get () in
         let datum = Datum (Obj.magic x) in
         (datum, marshalling_env)
     in
     let string_adapted_set : Unmarshalling_env.t -> marshallable -> unit * Unmarshalling_env.t =
       fun unmarshalling_env ->
         function
         | Datum x      -> ((real_set (Obj.magic x)), unmarshalling_env)
         | Pointer index -> assert false
     in
     { Fields_register.get = string_adapted_get; 
       Fields_register.set = string_adapted_set}

  method register_simple_field : 'a. ?name:string -> (unit -> 'a) -> ('a -> unit) -> unit =
     fun ?name real_get real_set ->
       self#register_string_adapted_accessors ?field_name:name (self#adapt_simple_field real_get real_set)

  (* -----------------------------
       Registering OBJECT fields
     ----------------------------- *)

  (* To be used with map_and_fold (map the first result, fold the second) *)
  method private marshallable_of_object
     : Marshalling_env.t -> (< marshaller:marshaller; .. > as 'a) -> marshallable * Marshalling_env.t
    = fun marshalling_env obj ->
    let oid = Oid (Oo.id obj) in
    (* The result is always a pointer (the string is stored in the marshalling_env): *)
    match Marshalling_env.search_index_by_oid oid marshalling_env with
    | Some index ->
        (if is_tracing_enable () then
           Printf.kfprintf flush stderr "marshallable_of_object: making a pointer to index %d\n" index);
        let pointer = (Pointer index) in
        (pointer, marshalling_env)
    | None ->
        let (marshalling_env, index) = 
          Marshalling_env.add_oid_and_get_index oid marshalling_env 
        in
        (if is_tracing_enable () then
          Printf.kfprintf flush stderr "marshallable_of_object: added object (oid = %d) with index %d\n" (Oo.id obj) index);
        let (str, marshalling_env) =
          obj#marshaller#protected_save_to_string_in_a_context marshalling_env
        in
        let marshalling_env =
          Marshalling_env.add_marshalled_object index str marshalling_env 
        in
        let pointer = (Pointer index) in
        (pointer, marshalling_env)

  method private object_of_marshallable (* : .. -> .. -> object * unmarshalling_env *) =
    fun ~object_getter_or_maker unmarshalling_env ->
      function
      | Pointer index ->
         begin
          match Unmarshalling_env.get_string_or_object_by_index index unmarshalling_env with
	  | Left (x:string) ->  
	      let obj = object_getter_or_maker () in
              (if is_tracing_enable () then
	        Printf.kfprintf flush stderr "object_of_marshallable: adding object (oid = %d) with index %d then unmarshalling\n" (Oo.id obj) index);
	      let unmarshalling_env = 
		Unmarshalling_env.replace_string_with_object index (Obj.magic obj) unmarshalling_env 
	      in
	      let (), unmarshalling_env =
		obj#marshaller#protected_load_from_string_in_a_context unmarshalling_env x
	      in
	      (obj, unmarshalling_env)
	  
	  | Right obj ->
              (if is_tracing_enable () then
	        Printf.kfprintf  flush stderr "object_of_marshallable: found object (oid = %d) with index %d\n" (Oo.id (Obj.magic obj)) index);
	      (Obj.magic obj), unmarshalling_env
         end (* of Pointer's case *)
      | Datum _ -> assert false
	  
	  
  method private adapt_object_field
     : (unit -> (< marshaller:marshaller; .. >)) ->  (* The real get-accessor of the field *)
       (< marshaller:marshaller; .. > -> unit) ->    (* The real set-accessor of the field *)
       Fields_register.string_adapted_accessors
     =
     fun real_get real_set ->
     let string_adapted_get : Marshalling_env.t -> unit -> marshallable * Marshalling_env.t =
       fun marshalling_env () ->
         let obj = real_get () in
         self#marshallable_of_object marshalling_env obj
     in
     let string_adapted_set : Unmarshalling_env.t -> marshallable -> unit * Unmarshalling_env.t =
       fun unmarshalling_env marshallable ->
         let (obj, unmarshalling_env) =
           self#object_of_marshallable 
             ~object_getter_or_maker:real_get 
             (unmarshalling_env)
             (marshallable)
         in
         ((real_set (Obj.magic obj)), unmarshalling_env)
     in
     { Fields_register.get = string_adapted_get; 
       Fields_register.set = string_adapted_set}

  method register_object_field
     : 'a. ?name:string -> 
           (unit -> (< marshaller:marshaller; .. > as 'a)) ->  (* The real get-accessor of the field *)
           ('a -> unit) ->                                     (* The real set-accessor of the field *)
           unit
     =
     Obj.magic begin
       fun ?name real_get real_set ->
         (self#register_string_adapted_accessors ?field_name:name (self#adapt_object_field real_get real_set))
       end

  (* -----------------------------------------
       Registering FUNCTORIZED OBJECT fields
     ----------------------------------------- *)

  method private adapt_functorized_object_field
     : 'object_t.
        (((< marshaller : marshaller; .. >) -> marshallable) -> 'object_t -> marshallable) -> (* functor_map1 *)
        ((marshallable -> < marshaller : marshaller; .. >) -> marshallable -> 'object_t) ->   (* functor_map2 *)
        (unit -> < marshaller : marshaller; .. >) ->                                           (* object_maker *)
        (unit -> 'object_t) ->                                                                 (* real_get     *)
        ('object_t -> unit) ->                                                                 (* real_set     *)
        Fields_register.string_adapted_accessors (* result *)
     =
     fun functor_map1 functor_map2 object_maker real_get real_set ->
     let string_adapted_get : Marshalling_env.t -> unit -> marshallable * Marshalling_env.t =
       fun marshalling_env () ->
         let object_t = real_get () in
         let marshallable_t, marshalling_env =
           (map_and_fold functor_map1) (self#marshallable_of_object) (marshalling_env) object_t
         in
         (Datum (Obj.magic marshallable_t), marshalling_env)
     in
     let string_adapted_set : Unmarshalling_env.t -> marshallable -> unit * Unmarshalling_env.t =
       fun unmarshalling_env ->
         function
         | Datum x ->
             let marshallable_t = Obj.magic x in
             let object_t, unmarshalling_env =
               (map_and_fold functor_map2)
                 (self#object_of_marshallable ~object_getter_or_maker:object_maker)
                 (unmarshalling_env)
                 marshallable_t
             in
             ((real_set object_t), unmarshalling_env)

         (* The field is a non-object structure containing objects *)
         | Pointer _ -> (assert false) 
     in
     { Fields_register.get = string_adapted_get; 
       Fields_register.set = string_adapted_set}


  method register_functorized_object_field
     : 'functor_map 'object_maker 'real_get 'real_set. ?name:string -> 'functor_map -> 'object_maker -> 'real_get -> 'real_set -> unit
     = Obj.magic begin
          fun ?name functor_map object_maker real_get real_set ->
            let map1 = Obj.magic functor_map in
            let map2 = Obj.magic functor_map in
            self#register_string_adapted_accessors ?field_name:name
              (self#adapt_functorized_object_field map1 map2 object_maker real_get real_set)
          end

  (* --------------------------------------
          S A V I N G    (marshalling)
     -------------------------------------- *)

  method save_to_string : string =
    let mystic_structure = self#save_to_mystic_structure in
    Marshal.to_string (mystic_structure) (_WITHOUT_CLOSURES_OF_COURSE)
    
  method save_to_file filename =
    let mystic_structure = self#save_to_mystic_structure in
    with_open_out_bin ~filename
      (fun out_channel ->
         Marshal.to_channel out_channel
           (mystic_structure) 
           (_WITHOUT_CLOSURES_OF_COURSE))
  
  method private save_to_mystic_structure : mystic_structure =
    let ((labelled_values, unlabelled_values), marshalling_env) =
      self#save_to_labelled_and_unlabelled_values
        (Marshalling_env.initial ~parent_oid:(Oo.id parent))
    in
    (* Extract now the index->string environment from the marshalling_env: *)
    let index_string_list = 
      Marshalling_env.extract_index_string_env marshalling_env 
    in
    { labelled_values=labelled_values; 
      unlabelled_values=unlabelled_values; 
      index_string_list=index_string_list; }
  
  method (* private *) protected_save_to_string_in_a_context marshalling_env : string * Marshalling_env.t =
    let ((labelled_values, unlabelled_values), marshalling_env) =
      self#save_to_labelled_and_unlabelled_values marshalling_env
    in
    let str = Marshal.to_string (labelled_values, unlabelled_values) (_WITHOUT_CLOSURES_OF_COURSE) in
    (str, marshalling_env)
  
  method private save_to_labelled_and_unlabelled_values 
    (marshalling_env : Marshalling_env.t) 
    : (labelled_values * unlabelled_values) * Marshalling_env.t
    =
    let labelled_values,   marshalling_env = self#save_to_labelled_values   (marshalling_env) in
    let unlabelled_values, marshalling_env = self#save_to_unlabelled_values (marshalling_env) in
    ((labelled_values, unlabelled_values), marshalling_env)
     
  method private save_to_labelled_values (marshalling_env) =
    let (result: (string*marshallable) list), marshalling_env =
      List.fold_left
	(fun state ((name:string), (saa:Fields_register.string_adapted_accessors)) ->
	    let result, marshalling_env = state in
	    let (mshlable, marshalling_env) = saa.Fields_register.get marshalling_env () in
	    let state' = ((name, mshlable)::result, marshalling_env) in
	    state')
	([], marshalling_env)
	self#get_named_fields_as_ordered_assoc_list (* NAMED FIELDS! *)
    in
    (result, marshalling_env)
  
  method private save_to_unlabelled_values (marshalling_env) =
    let (result: marshallable list), marshalling_env =
      List.fold_left
	(fun state (saa:Fields_register.string_adapted_accessors) ->
	    let result, marshalling_env = state in
	    let (mshlable, marshalling_env) = saa.Fields_register.get marshalling_env () in
	    let state' = (mshlable::result, marshalling_env) in
	    state')
	([], marshalling_env)
	self#get_anonymous_fields (* ANONYMOUS FIELDS! *)
    in
    (List.rev result, marshalling_env)

 
  (* --------------------------------------
         L O A D I N G   (unmarshalling)
     -------------------------------------- *)

  (* Loading from a string: *)   
  method load_from_string ?mapping (str:string) : unit =
    let mystic_structure = (Marshal.from_string str 0) in
    self#load_from_mystic_structure ?mapping mystic_structure

  (* Loading from a file: *)   
  method load_from_file ?mapping filename : unit =
    let (mystic_structure : mystic_structure) =
      try
        with_open_in_bin ~filename
          (fun in_channel length ->
             Marshal.from_channel in_channel)
      with _ ->
        failwith "load_from_file: failed unmarshalling the file content"
    in
    self#load_from_mystic_structure ?mapping (mystic_structure)

  (* Loading from a mystic_structure: *)   
  method private load_from_mystic_structure : ?mapping:(string->string) -> mystic_structure -> unit = 
    fun ?mapping mystic_structure ->
      let index_string_list = mystic_structure.index_string_list in
      let unmarshalling_env = 
        Unmarshalling_env.initial ?mapping ~parent:(Obj.magic parent) ~index_string_list ()
      in
      let (), _unmarshalling_env = 
	self#load_from_labelled_and_unlabelled_values 
	  (unmarshalling_env) 
	  (mystic_structure.labelled_values)
	  (mystic_structure.unlabelled_values)
      in
      ()

  method (* private *) protected_load_from_string_in_a_context (unmarshalling_env) (str:string) 
    : unit * Unmarshalling_env.t 
    =
    let (labelled_values, unlabelled_values) =
      try
        Marshal.from_string str 0
      with _ ->
        failwith "protected_load_from_string_in_a_context: failed unmarshalling the string"
    in
    self#load_from_labelled_and_unlabelled_values (unmarshalling_env) (labelled_values) (unlabelled_values)
    
  (* Loading from a both labelled and unlabelled values. 
     It's simply the composition of the two functions loading 
     from labelled and from unlabelled values: *)
  method private load_from_labelled_and_unlabelled_values 
    (unmarshalling_env) 
    (labelled_values : (label * marshallable) list)
    (unlabelled_values : marshallable list)
    : unit * Unmarshalling_env.t 
    =
    let (), unmarshalling_env = self#load_from_labelled_values   (unmarshalling_env) (labelled_values)   in
    let (), unmarshalling_env = self#load_from_unlabelled_values (unmarshalling_env) (unlabelled_values) in
    ((), unmarshalling_env)
      
  (* Loading from a list of labelled values: *)
  method private load_from_labelled_values
    (unmarshalling_env) 
    (labelled_values : (label * marshallable) list)
    : unit * Unmarshalling_env.t 
    =
    let set_arg_list =
      let saa_arg_list =
        Fields_register.match_named_fields_with_labelled_values
          ?class_name:(self#parent_class_name)
          ?label_mapping:(Unmarshalling_env.extract_label_mapping unmarshalling_env)
          (self#get_fields_register) 
          (labelled_values)
      in
      List.map (fun (saa,arg) -> (saa.Fields_register.set,arg)) saa_arg_list
    in
    self#load_from_set_arg_list (unmarshalling_env) set_arg_list

  (* Loading from a list of unlabelled values: *)
  method private load_from_unlabelled_values
    (unmarshalling_env) 
    (unlabelled_values : marshallable list)
    : unit * Unmarshalling_env.t 
    =
    let set_arg_list =
      let saa_arg_list =
        Fields_register.match_anonymous_fields_with_unlabelled_values
          ?class_name:(self#parent_class_name)
          (self#get_fields_register)
          (unlabelled_values)
      in
      List.map (fun (saa,arg) -> (saa.Fields_register.set,arg)) saa_arg_list
    in
    self#load_from_set_arg_list (unmarshalling_env) set_arg_list
    
  method private load_from_set_arg_list (unmarshalling_env) set_arg_list : unit * Unmarshalling_env.t =
    let unmarshalling_env =
      List.fold_left
	(fun unmarshalling_env (set, arg) -> snd (set unmarshalling_env arg))
	unmarshalling_env
	set_arg_list
    in
    ((), unmarshalling_env)
            
  (* --------------------------------------
              Other methods 
     -------------------------------------- *)
     
   method compare : 'a. (< marshaller:marshaller; .. > as 'a) -> int
     = fun obj -> Pervasives.compare (self#save_to_string) (obj#marshaller#save_to_string)

   method equals : 'a. (< marshaller:marshaller; .. > as 'a) -> bool
     = fun obj -> (self#save_to_string) = (obj#marshaller#save_to_string)

   method hash : int =
     Hashtbl.hash (self#save_to_string)

   method md5sum : string =
     Digest.to_hex (Digest.string (self#save_to_string))

end;;

IFDEF DOCUMENTATION_OR_DEBUGGING THEN
module Example = struct

(* A concrete syntax like: 

     tag (marshallable) class <class-definitions> 
   
   adds the parameter ?marshaller to all defined classes as first parameter *)
class class1 ?(marshaller:(marshaller option ref) option) () =
let marshaller = match marshaller with None -> ref None | Some r -> r in
object (self)

  (* Automatically added at the beginning of the class definition: *)
  inherit marshallable_class ~name:"class1" ~marshaller ()

  (* When a class is inherited, the parameter ~marshaller is given to the class constructor: 
     inherit class0 expr  =>  inherit ~marshaller class0 expr 
     *)
  
  val mutable field0 = 16
  method get_field0 = field0
  method set_field0 v = field0 <- v
  initializer
    self#marshaller#register_simple_field ~name:"field0" (fun () -> self#get_field0) self#set_field0;
    
  val mutable field1 = "field1"
  method get_field1 = field1
  method set_field1 v = field1 <- v
  initializer
    self#marshaller#register_simple_field ~name:"field1" (fun () -> self#get_field1) self#set_field1;

  val mutable field2 = Some 42
  method get_field2 = field2
  method set_field2 v = field2 <- v
  initializer
    self#marshaller#register_simple_field ~name:"field2" (fun () -> self#get_field2) self#set_field2;
  
  
end (* class1 *) 


class class2 ?(marshaller:(marshaller option ref) option) () =
let marshaller = match marshaller with None -> ref None | Some r -> r in
object (self)

  (* Automatically added at the beginning of the class definition: *)
  inherit marshallable_class ~name:"class2" ~marshaller ()
    
  (* Share the marshaller: *)
  inherit class1 ~marshaller ()
  
  (* A field containing a (marshallable) object:
     Concrete syntax:
       tag (object) val mutable field3 : class1 = new class1 ()
     In case of composition (not inheritance) I dont need to provide
     the same marshaller to the class constructor. *)
  val mutable field3 : class1 = new class1 () (* another marshaller! *)
  method get_field3 = field3
  method set_field3 v = field3 <- v
  initializer
    self#marshaller#register_object_field ~name:"field3" (fun () -> self#get_field3) self#set_field3;

  (* A field containing an optional (marshallable) object: 
     Concrete syntax:
       tag (object option) val mutable field4 : class2 option = None
    *)
  val mutable field4 : class2 option = None
  method get_field4 = field4
  method set_field4 v = field4 <- v
  initializer
    let object_maker () = new class2 () in (* from the tag *)
    let functor_map = Option.map in        (* from the tag: option -> Option.map *)
    self#marshaller#register_functorized_object_field ~name:"field4"
      functor_map
      object_maker
      (fun () -> self#get_field4)
      self#set_field4
  
  (* A field containing a list of (marshallable) objects: 
     Concrete syntax:
       tag (object list) val mutable field4 : class2 list = None
  *)
  val mutable field5 : class2 list = []
  method get_field5 = field5
  method set_field5 v = field5 <- v
  initializer
    let object_maker () = new class2 () in (* from the tag *)
    let functor_map = List.map in          (* from the tag: list -> List.map *)
    self#marshaller#register_functorized_object_field ~name:"field5"
      functor_map
      object_maker
      (fun () -> self#get_field5)
      self#set_field5

end (* class2 *)

class class3 ?(marshaller:(marshaller option ref) option) () =
let marshaller = match marshaller with None -> ref None | Some r -> r in
object (self)

  (* Automatically added at the beginning of the class definition: *)
  inherit marshallable_class ~name:"class3" ~marshaller ()
    
  (* Note that now `marshaller' stands for the inherited field (not the parameter): *)
  inherit class2 ~marshaller ()

  val mutable field6 = '6'
  method get_field6 = field6
  method set_field6 v = field6 <- v
  initializer
    self#marshaller#register_simple_field ~name:"field6" (fun () -> self#get_field6) self#set_field6;

end (* class3 *)


let crash_test () = 
  let x  = new class3 () in
  let y  = new class3 () in
  (assert ((x=y) = false));
  (* but *)
  (assert (x#marshaller#equals y));
  let z1  = new class2 () in
  let z2  = new class2 () in
  let load_y_with_x () = 
    y#marshaller#load_from_string (x#marshaller#save_to_string)
  in
  x#set_field5 [z1; z2];
  (assert (not (x#marshaller#equals y)));
  load_y_with_x ();
  (assert (x#marshaller#equals y));
  z1#set_field0 1234;
  (assert (not (x#marshaller#equals y)));
  let z1' = List.hd y#get_field5 in
  (assert (not (z1#marshaller#equals z1')));
  z1'#set_field0 1234;
  (assert (z1#marshaller#equals z1'));
  (assert (x#marshaller#equals y));
  (* Success: *)
  ()
;;

end (* module Example *)
ENDIF