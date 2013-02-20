(* This file is part of ocamlbricks
   Copyright (C) 2013  Jean-Vincent Loddo

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


module Mutex_group = struct

  module Mutex_set = Set.Make (struct type t = Mutex.t let compare = compare end)

  let merge_and_sort_two_lists l1 l2 =
    let s = List.fold_left (fun s x -> Mutex_set.add x s) Mutex_set.empty l1 in
    let s = List.fold_left (fun s x -> Mutex_set.add x s) s l2 in
    Mutex_set.elements s

  let merge_and_sort_all_lists l1 ls =
    let s = List.fold_left (fun s x -> Mutex_set.add x s) Mutex_set.empty l1 in
    let add s l2 = List.fold_left (fun s x -> Mutex_set.add x s) s l2 in
    let s = List.fold_left (add) s ls in
    Mutex_set.elements s

  type t =
  | Single of Mutex.t
  | Group  of Mutex.t * (Mutex.t list) * (Mutex.t list)

  let mutex_list_of = function
  | Single m              -> [m]
  | Group (frame, ms, ws) -> ms

  let single () =
    let frame = Mutex.create () in
    Single frame

  let group t1 t2 =
    let ms =
      let ms1 = mutex_list_of t1 in
      let ms2 = mutex_list_of t2 in
      merge_and_sort_two_lists ms1 ms2
    in
    match ms with
    | m::[] -> Single m
    | _ ->
      begin
        let frame = Mutex.create () in
        let ws = List.rev ms in
        Group (frame, ms, ws)
      end

  let group_from_list1 (t1,ts) =
    let ms =
      let ms1 = mutex_list_of t1 in
      let msl = List.map mutex_list_of ts in
      merge_and_sort_all_lists ms1 msl
    in
    match ms with
    | m::[] -> Single m
    | _ ->
      begin
        let frame = Mutex.create () in
        let ws = List.rev ms in
        Group (frame, ms, ws)
      end

  let lock = function
  | Single m -> Mutex.lock m
  | Group (frame, ms, ws) ->
      begin
        Mutex.lock frame;
        List.iter Mutex.lock ms;
      end

  let unlock = function
  | Single m -> Mutex.unlock m
  | Group (frame, ms, ws) ->
      begin
        List.iter Mutex.unlock ws;
        Mutex.unlock frame;
      end

  let wait alert = function
  | Single m -> Condition.wait alert m
  | Group (frame, ms, ws) ->
      begin
        List.iter Mutex.unlock ws;
        Condition.wait alert frame;
        List.iter Mutex.lock ms;
      end

  let with_mutex (t:t) thunk =
   lock t;
   try
     let result = thunk () in
     unlock t;
     result
   with e -> begin
     unlock t;
     raise e;
   end

 end (* Mutex_group *)


(* Mutexes and related conditions are ordered by mutexes simply with Pervasives.compare.
   The inner evaluation is unprotected. *)
type 'state t = {
 mutexes            : Mutex_group.t;
 alert_on_commit    : Condition.t;
 get_content        : unit -> 'state;
 propose_content    : 'state -> 'state * bool;
 revno              : int ref;
 waiting_no         : int ref;
 equality           : 'state -> 'state -> bool;
 on_proposal        : ('state -> 'state -> 'state) Thunk.fifo_container;
 (* "Private methods": *)
 on_proposal_container_unchanged : unit -> bool;
 no_longer_in_use   : bool ref;
 equals_to_current  : ('state -> bool) ref;
}
;;

let copy_but_already_protected_by ~mutexes t =
{ t with mutexes = mutexes; }
;;

let make
 ?mutexes
 ?(equality:('state -> 'state -> bool) option)
 ?(on_proposal:('state -> 'state -> 'state) option)
 ~(get_content:(unit -> 'state))
 ?(set_content:('state -> unit) option)
 ?(propose_content:('state -> 'state * bool) option)
 ()
 : 'state t
 =
 let mutexes =
   match mutexes with
   | None         -> Mutex_group.single ()
   | Some mutexes -> mutexes
 in
 let equality =
   match equality with
   | None -> fun s s' -> (s == s' || s = s')
   | Some equality -> equality
 in
 (* Will be updated during commit: *)
 let initial_equals_to_current =
   let current = get_content () in
   (* The following partial application of the `equality' predicate is very relevant
      when the content is a reference (for example an object or a cortex): *)
   equality current
 in
 (* Work around for a strange problem of the type-checker about the recursive definition of `self': *)
 let (is_propose_content_provided, propose_content, set_content) =
   let unused = (fun proposal -> assert false) in
   match set_content, propose_content with
   | None    , (Some f) -> (true, f, unused)
   | (Some f), None     -> (false, unused, f)
   | _, _               -> invalid_arg "Cortex.make: ~set_content xor ~propose_content must be provided."
 in
 (* The record is morally an object: *)
 let rec self : 'state t =
  let alert_on_commit = Condition.create () in
  let on_proposal =
    let container = new Thunk.fifo_container ~fallback:(fun () -> fun s0 s1 -> s1) () in
    let () = Option.iter (fun f -> ignore (container#register_thunk (fun () -> f))) on_proposal in
    container
  in
  let on_proposal_container_unchanged =
    let previous_revno = ref 0 in
    fun () ->
      let current = self.on_proposal#revno in
      let result = (current = !previous_revno) in
      previous_revno := current;
      result
  in
  let propose_content (proposal) =
    if is_propose_content_provided then propose_content (proposal) else
    let equals_to_previous = self.equals_to_current in
    let () = set_content (proposal) in
    let accepted = get_content () in
    if !equals_to_previous (accepted)
      then (accepted, false)
      else (accepted, true)
  in
  { mutexes           = mutexes;
    alert_on_commit   = alert_on_commit;
    get_content       = get_content;
    propose_content   = propose_content;
    revno             = ref 0;
    waiting_no        = ref 0;
    equality          = equality;
    equals_to_current = ref initial_equals_to_current;
    on_proposal       = on_proposal;
    on_proposal_container_unchanged = on_proposal_container_unchanged;
    no_longer_in_use  = ref false;
    }
 (* end of self definition *)
 in
 self


let revno_equality x =
  let r = x.revno in
  (fun x' -> x==x' && x'.revno = r)

let revno_or_content_equality : 'a t -> 'a t -> bool =
  fun x1 ->
    let r = x1.revno in
    fun x2 ->
      (x1==x2 && x2.revno = r) ||
      (let v1 = x1.get_content () in
       let v2 = x2.get_content () in
       (x1.equality v1 v2) && (x2.equality v2 v1))


(* The universal method (unprotected and without guards): *)
let unprotected_eval : 'a 'b. ('state -> 'a -> 'state * ('state -> 'b)) -> 'a -> 'state t -> 'b * bool =
  fun f a t ->
    let current = t.get_content () in
    let equals_to_current = !(t.equals_to_current) in
    let (first_proposal, b_of_state) =
      (* Apply the update-proposal `f' *)
      f current a
    in
    if (equals_to_current first_proposal) && (t.on_proposal_container_unchanged ())
      then
	(* No changes => no callbacks *)
	((b_of_state current), false)
      else begin
	let rec local_fixpoint s () =
	  (* Again a partial application: *)
	  let equals_to_s = (t.equality s) in
	  let s' =
	    (* Callbacks raising an exception are ignored: *)
	    t.on_proposal#apply ~folder:(fun state f -> try f current state with _ -> state) s
	  in
	  if equals_to_s (s') then s else local_fixpoint s' ()
	in
	let fixed_proposal = local_fixpoint first_proposal () in
	if (equals_to_current fixed_proposal)
	  then
	    (* No changes => callbacks are finally agreed for the current content *)
	    ((b_of_state current), false)
	  else begin
	    (* A change should be observed, supposing the provided `set_content'
	       or 'propose_content' agreed with the `fixed_proposal': *)
	    let (accepted_proposal, changed) = t.propose_content (fixed_proposal) in
            if changed then
	      begin
	        (* A patch is really happened: *)
		t.revno := !(t.revno) + 1;
		t.equals_to_current := (t.equality accepted_proposal);
		if !(t.waiting_no) > 0
		  then begin
		    Condition.broadcast (t.alert_on_commit);
		    t.waiting_no := 0
		  end;
	      end; (* if changed *)
	    let result = ((b_of_state accepted_proposal), changed) in
	    result
	  end (* A change should be observed *)
      end (* A fixed proposal has been calculated *)

(* The `propose' specific case: *)
let unprotected_eval_propose : 'state -> 'state t -> 'state * bool =
  fun s1 t -> unprotected_eval (fun s0 s1 -> s1, (fun s2 -> s2)) s1 t

(* The universal method (protected and guarded version): *)
let eval : 'a 'b. ?guard:('state -> bool) -> ('state -> 'a -> 'state * ('state -> 'b)) -> 'a -> 'state t -> 'b * bool =
  fun ?guard f a t ->
    match guard with
    | None -> Mutex_group.with_mutex t.mutexes (fun () -> unprotected_eval f a t)
    | Some guard ->
	Mutex_group.with_mutex t.mutexes
	  (fun ()  ->
	    if guard (t.get_content ())
	      then unprotected_eval f a t
	      else begin
		incr (t.waiting_no);
		Mutex_group.wait (t.alert_on_commit) t.mutexes;
		while not (guard (t.get_content ())) do
		  incr (t.waiting_no);
		  Mutex_group.wait (t.alert_on_commit) t.mutexes
		done;
		unprotected_eval f a t
	      end)

let eval_get ?guard t = fst (eval ?guard (fun s () -> s, (fun s -> s)) () t)
let eval_set ?guard s1 t = ignore (eval ?guard (fun s0 s1 -> s1, (fun s2 -> s2)) s1 t)
let eval_propose ?guard s1 t = eval ?guard (fun s0 s1 -> s1, (fun s2 -> s2)) s1 t
let eval_move ?guard f t = eval ?guard (fun s0 () -> (f s0), (fun s2 -> s2)) () t

(* Flipped versions: *)
let get ?guard t = fst (eval ?guard (fun s () -> s, (fun s -> s)) () t)
let set ?guard t s1 = ignore (eval ?guard (fun s0 s1 -> s1, (fun s2 -> s2)) s1 t)
let propose ?guard t s1 = eval ?guard (fun s0 s1 -> s1, (fun s2 -> s2)) s1 t
let move ?guard t f = eval ?guard (fun s0 () -> (f s0), (fun s2 -> s2)) () t
let apply ?guard t f b = fst (eval ?guard (fun s b -> s, (fun s -> f s b)) b t)


(* Flipped and asynchronous versions: *)
module Async = struct

  let set ?guard t s1 =
    ignore (Thread.create (fun () -> eval ?guard (fun s0 s1 -> s1, (fun s2 -> s2)) s1 t) ())

  let move ?guard t f =
    ignore (Thread.create (fun () -> eval ?guard (fun s0 () -> (f s0), (fun s2 -> s2)) () t) ())

end (* module Async *)

let eval_propose_on_sub_cortex_commit : (unit -> 'state) -> 'sub_state t -> 'state t -> 'state * bool =
  fun set sub t ->
    let f = (fun s0 s1 -> s1, (fun s2 -> s2)) in
    Mutex_group.with_mutex t.mutexes
      (fun () ->
         incr sub.waiting_no;
         Mutex_group.wait (sub.alert_on_commit) t.mutexes;
         unprotected_eval f (set ()) t)

(* May be also called `unit' or `create': 'a -> 'a t *)
let return
 ?(equality:('state -> 'state -> bool) option)
 ?(on_proposal:('state -> 'state -> 'state) option)
 (content:'state)
 =
 let cell = ref content in
 let get_content () = !cell in
 let set_content v = (cell := v) in
 make ?equality ?on_proposal ~get_content ~set_content ()

let of_object
 ?(equality:('state -> 'state -> bool) option)
 ?(on_proposal:('state -> 'state -> 'state) option)
 (x:< get:'state; set:'state -> unit; >)
 =
 let get_content () = x#get in
 let set_content v = x#set v in
 make ?equality ?on_proposal ~get_content ~set_content ()


(* Ci vogliono i mutex ricorsivi? Negli on_proposal vorrei utilizzare qualunque funzione protetta
   sul cortex contenuto (per esempio Process.stop). A meno di poter creare un oggetto uguale a quello contenuto
   ma non protetto: una specie di copia che in realtà non è una copia ma piuttosto una "vista" dell'originale.
   La cosa potrebbe essere ottenuta trasformando i campi mutabili in ref, in modo da poter copiare i ref, tranne i mutex
   che a questo punto potrebbero essere vuoti.
   *)
let duplicate ?on_proposal (x:'a t) : ('a t) t =
  let external_mutex   = Mutex_group.single () in
  let internal_mutexes = x.mutexes in
  (* Must be recreated on content changes: *)
  let mutexes =
    Mutex_group.group (external_mutex) (internal_mutexes)
  in
  let equality = revno_equality in
  let content_copy = ref x in
  let get_content () = !content_copy in
  let get_up_to_date_content = get_content in
  let set_content y = (content_copy := y) in
  let result = make ~mutexes ~equality ?on_proposal ~get_content ~set_content () in
  (* Will be protected: *)
  let rec trigger_on (sub_cortex) =
    let (_s,_b) = eval_propose_on_sub_cortex_commit (get_up_to_date_content) (sub_cortex) (result) in
    if !(result.no_longer_in_use) then () else begin
    IFDEF DOCUMENTATION_OR_DEBUGGING THEN (assert (equality _s !content_copy)) ENDIF;
    trigger_on (sub_cortex);
    end
  in
  let _thd1 = Thread.create (trigger_on) x in
  result

let group_single ?on_proposal (x:'a t) : ('a) t =
  let mutexes = x.mutexes in
  let equality = x.equality in
  let get_up_to_date_content () = x.get_content () in
  let content_copy = ref (get_up_to_date_content ()) in
  let get_content () = !content_copy in
  let rec propose_content v =
    begin
      content_copy := v;
      let (v', b) = unprotected_eval_propose v x in
      if b
        then unprotected_eval_propose v' (Lazy.force result)
        else (v, false)
    end
  and
    result = lazy (make ~mutexes ~equality ?on_proposal ~get_content ~propose_content ())
  in
  let result = (Lazy.force result) in
  (* Will be protected: *)
  let rec trigger_on (sub_cortex) =
    let (_s,_b) = eval_propose_on_sub_cortex_commit (get_up_to_date_content) (sub_cortex) (result) in
    if !(result.no_longer_in_use) then () else begin
    IFDEF DOCUMENTATION_OR_DEBUGGING THEN (assert (equality _s !content_copy)) ENDIF;
    trigger_on (sub_cortex);
    end
  in
  let _thd1 = Thread.create (trigger_on) x in
  result

(*let group_single_mutable ?on_proposal (x:'a t) : ('a t) t =
  let equality = revno_equality in

  let get_up_to_date_content () = x.get_content () in
  let content_copy = ref (get_up_to_date_content ()) in
  let get_content () = !content_copy in
  let rec propose_content v =
    begin
      content_copy := v;
      let (v', b) = unprotected_eval_propose v x in
      if b
        then unprotected_eval_propose v' (Lazy.force result)
        else (v, false)
    end
  and
    result = lazy (make ~mutexes ~equality ?on_proposal ~get_content ~propose_content ())
  in
  (* Will be protected: *)
  let rec trigger_on (sub_cortex) =
    let (s,b) = eval_propose_on_sub_cortex_commit (get_up_to_date_content) (sub_cortex) (Lazy.force result) in
    if (Lazy.force result).no_longer_in_use then () else begin
    IFDEF DOCUMENTATION_OR_DEBUGGING THEN (assert (equality s !content_copy)) ENDIF;
    trigger_on (sub_cortex);
    end
  in
  let _thd1 = Thread.create (trigger_on) x in
  Lazy.force result*)

let group_pair ?unprotect ?on_proposal (x:'a t) (y:'b t) : ('a * 'b) t =
  let mutexes = Mutex_group.group x.mutexes y.mutexes in
(*   Mutex_group.with_mutex mutexes (fun () -> ....tutto il resto!!!! se unprotect=None ) *)
  let equality (a,b) =
    let equals_a = (x.equality a) in
    let equals_b = (y.equality b) in
    fun (a',b') -> (equals_a a') && (equals_b b')
  in
  let get_up_to_date_content () = (x.get_content (), y.get_content ()) in
  let content_copy = ref (get_up_to_date_content ()) in
  let get_content () = !content_copy in
  let rec propose_content (v1,v2) =
    begin
      content_copy := (v1,v2);
      let (v1', b1) = unprotected_eval_propose v1 x in
      let (v2', b2) = unprotected_eval_propose v2 y in
      if b1 || b2
        then unprotected_eval_propose (v1',v2') (Lazy.force result)
        else ((v1,v2), false)
    end
  and
    result = lazy (make ~mutexes ~equality ?on_proposal ~get_content ~propose_content ())
  in
  let result = (Lazy.force result) in
  (* Will be protected: *)
  let rec trigger_on (sub_cortex) =
    let (_s,_b) = eval_propose_on_sub_cortex_commit (get_up_to_date_content) (sub_cortex) (result) in
    if !(result.no_longer_in_use) then (Printf.kfprintf flush stderr "trigger_on: no_longer_in_use!\n"; ()) else begin
    IFDEF DOCUMENTATION_OR_DEBUGGING THEN (assert (equality _s !content_copy)) ENDIF;
    trigger_on (sub_cortex);
    end
  in
  let _thd1 = Thread.create (trigger_on) x in
  let _thd2 = Thread.create (trigger_on) y in
  result


let ungroup : ?unprotect:unit -> 'generic t -> unit =
  fun ?unprotect t ->
    let action () =
      t.no_longer_in_use := false;
      Container.Queue_with_identifiers.clear (t.on_proposal#as_queue)
    in
    if unprotect=Some ()
     then action ()
     else Mutex_group.with_mutex t.mutexes (action)


let group_triple ?on_proposal (x1:'a t) (x2:'b t) (x3:'c t) : ('a * 'b * 'c) t =
  let mutexes = Mutex_group.group_from_list1 (x1.mutexes,[x2.mutexes; x3.mutexes]) in
  let equality (a,b,c) =
    let equals_a = (x1.equality a) in
    let equals_b = (x2.equality b) in
    let equals_c = (x3.equality c) in
    fun (a',b',c') -> (equals_a a') && (equals_b b') && (equals_c c')
  in
  let get_up_to_date_content () = (x1.get_content (), x2.get_content (), x3.get_content ()) in
  let content_copy = ref (get_up_to_date_content ()) in
  let get_content () = !content_copy in
  let rec propose_content (v1,v2,v3) =
    begin
      content_copy := (v1,v2,v3);
      let (v1', b1) = unprotected_eval_propose v1 x1 in
      let (v2', b2) = unprotected_eval_propose v2 x2 in
      let (v3', b3) = unprotected_eval_propose v3 x3 in
      if b1 || b2 || b3
        then unprotected_eval_propose (v1',v2', v3') (Lazy.force result)
        else ((v1,v2,v3), false)
    end
  and
    result = lazy (make ~mutexes ~equality ?on_proposal ~get_content ~propose_content ())
  in
  let result = (Lazy.force result) in
  (* Will be protected: *)
  let rec trigger_on (sub_cortex) =
    let (_s,_b) = eval_propose_on_sub_cortex_commit (get_up_to_date_content) (sub_cortex) (result) in
    if !(result.no_longer_in_use) then () else begin
    IFDEF DOCUMENTATION_OR_DEBUGGING THEN (assert (equality _s !content_copy)) ENDIF;
    trigger_on (sub_cortex);
    end
  in
  let _thd1 = Thread.create (trigger_on) x1 in
  let _thd2 = Thread.create (trigger_on) x2 in
  let _thd3 = Thread.create (trigger_on) x3 in
  result


let group_quadruple ?on_proposal (x1:'a t) (x2:'b t) (x3:'c t) (x4:'d t) : ('a * 'b * 'c * 'd) t =
  let mutexes = Mutex_group.group_from_list1 (x1.mutexes,[x2.mutexes; x3.mutexes; x4.mutexes]) in
  let equality (a,b,c,d) =
    let equals_a = (x1.equality a) in
    let equals_b = (x2.equality b) in
    let equals_c = (x3.equality c) in
    let equals_d = (x4.equality d) in
    fun (a',b',c',d') -> (equals_a a') && (equals_b b') && (equals_c c') && (equals_d d')
  in
  let get_up_to_date_content () = (x1.get_content (), x2.get_content (), x3.get_content (), x4.get_content ()) in
  let content_copy = ref (get_up_to_date_content ()) in
  let get_content () = !content_copy in
  let rec propose_content (v1,v2,v3,v4) =
    begin
      content_copy := (v1,v2,v3,v4);
      let (v1', b1) = unprotected_eval_propose v1 x1 in
      let (v2', b2) = unprotected_eval_propose v2 x2 in
      let (v3', b3) = unprotected_eval_propose v3 x3 in
      let (v4', b4) = unprotected_eval_propose v4 x4 in
      if b1 || b2 || b3 || b4
        then unprotected_eval_propose (v1',v2',v3',v4') (Lazy.force result)
        else ((v1,v2,v3,v4), false)
    end
  and
    result = lazy (make ~mutexes ~equality ?on_proposal ~get_content ~propose_content ())
  in
  let result = (Lazy.force result) in
  (* Will be protected: *)
  let rec trigger_on (sub_cortex) =
    let (_s,_b) = eval_propose_on_sub_cortex_commit (get_up_to_date_content) (sub_cortex) result in
    if !(result.no_longer_in_use) then () else begin
    IFDEF DOCUMENTATION_OR_DEBUGGING THEN (assert (equality _s !content_copy)) ENDIF;
    trigger_on (sub_cortex);
    end
  in
  let _thd1 = Thread.create (trigger_on) x1 in
  let _thd2 = Thread.create (trigger_on) x2 in
  let _thd3 = Thread.create (trigger_on) x3 in
  let _thd4 = Thread.create (trigger_on) x4 in
  result

let group_quintuple ?on_proposal (x1:'a t) (x2:'b t) (x3:'c t) (x4:'d t) (x5:'e t) : ('a * 'b * 'c * 'd * 'e) t =
  let mutexes = Mutex_group.group_from_list1 (x1.mutexes,[x2.mutexes; x3.mutexes; x4.mutexes; x5.mutexes]) in
  let equality (a,b,c,d,e) =
    let equals_a = (x1.equality a) in
    let equals_b = (x2.equality b) in
    let equals_c = (x3.equality c) in
    let equals_d = (x4.equality d) in
    let equals_e = (x5.equality e) in
    fun (a',b',c',d',e') -> (equals_a a') && (equals_b b') && (equals_c c') && (equals_d d') && (equals_e e')
  in
  let get_up_to_date_content () = (x1.get_content (), x2.get_content (), x3.get_content (), x4.get_content (), x5.get_content ()) in
  let content_copy = ref (get_up_to_date_content ()) in
  let get_content () = !content_copy in
  let rec propose_content (v1,v2,v3,v4,v5) =
    begin
      content_copy := (v1,v2,v3,v4,v5);
      let (v1', b1) = unprotected_eval_propose v1 x1 in
      let (v2', b2) = unprotected_eval_propose v2 x2 in
      let (v3', b3) = unprotected_eval_propose v3 x3 in
      let (v4', b4) = unprotected_eval_propose v4 x4 in
      let (v5', b5) = unprotected_eval_propose v5 x5 in
      if b1 || b2 || b3 || b4 || b5
        then unprotected_eval_propose (v1',v2',v3',v4',v5') (Lazy.force result)
        else ((v1,v2,v3,v4,v5), false)
    end
  and
    result = lazy (make ~mutexes ~equality ?on_proposal ~get_content ~propose_content ())
  in
  let result = (Lazy.force result) in
  (* Will be protected: *)
  let rec trigger_on (sub_cortex) =
    let (_s,_b) = eval_propose_on_sub_cortex_commit (get_up_to_date_content) (sub_cortex) (result) in
    if !(result.no_longer_in_use) then () else begin
    IFDEF DOCUMENTATION_OR_DEBUGGING THEN (assert (equality _s !content_copy)) ENDIF;
    trigger_on (sub_cortex);
    end
  in
  let _thd1 = Thread.create (trigger_on) x1 in
  let _thd2 = Thread.create (trigger_on) x2 in
  let _thd3 = Thread.create (trigger_on) x3 in
  let _thd4 = Thread.create (trigger_on) x4 in
  let _thd5 = Thread.create (trigger_on) x5 in
  result


let on_proposal_append t thunk =
  Mutex_group.with_mutex t.mutexes
    (fun () ->
       t.on_proposal#register_thunk (fun () -> thunk))

let on_proposal_remove t id =
  Mutex_group.with_mutex t.mutexes
    (fun () ->
       t.on_proposal#remove id)

let on_proposal_clear t =
  Mutex_group.with_mutex t.mutexes
    (fun () ->
       Container.Queue_with_identifiers.clear (t.on_proposal#as_queue))


let group_array ?on_proposal (xs:('a t) array) : ('a array) t =
  let size = Array.length xs in
  if size = 0 then invalid_arg "Cortex.group_array: empty array" else
  let xl = Array.to_list xs in
  let mutexes =
    let head = List.hd xl in
    let tail = List.tl xl in
    Mutex_group.group_from_list1
      (head.mutexes, (List.map (fun x->x.mutexes) tail))
  in
  (* Utility for folding boolean results with the logical operator (&&): *)
  let and_foldi f vs =
    let rec loop i =
      if i>=size then true else
      if f i vs.(i)
        then loop (i+1)
        else false (* stop immediately! *)
    in loop 0
  in
  let or_foldi f vs =
    let rec loop i =
      if i>=size then false else
      if f i vs.(i)
        then true (* stop immediately! *)
        else loop (i+1)
    in loop 0
  in
  let equality vs =
    let equals_v = Array.mapi (fun i v -> xs.(i).equality v) vs in
    and_foldi (fun i v' -> equals_v.(i) v')
  in
  let get_up_to_date_content () = Array.map (fun x -> x.get_content ()) xs in
  let content_copy = ref (get_up_to_date_content ()) in
  let get_content () = !content_copy in
  let rec propose_content vs =
    begin
      content_copy := vs;
      let v'bs = Array.mapi (fun i v -> unprotected_eval_propose v xs.(i)) vs in
      let someone_changed = or_foldi (fun i (v',b) -> b) v'bs in
      if someone_changed
        then unprotected_eval_propose (Array.map fst v'bs) (Lazy.force result)
        else (vs, false)
    end
  and
    result = lazy (make ~mutexes ~equality ?on_proposal ~get_content ~propose_content ())
  in
  let result = (Lazy.force result) in
  (* Will be protected: *)
  let rec trigger_on (sub_cortex) =
    let (_s,_b) = eval_propose_on_sub_cortex_commit (get_up_to_date_content) (sub_cortex) (result) in
    if !(result.no_longer_in_use) then () else begin
    IFDEF DOCUMENTATION_OR_DEBUGGING THEN (assert (equality _s !content_copy)) ENDIF;
    trigger_on (sub_cortex);
    end
  in
  let _threads = Array.map (fun x -> Thread.create (trigger_on) x) xs in
  result



class type ['a] public_interface = object
  method eval    : 'b 'c. ?guard:('a -> bool) -> ('a -> 'b -> 'a * ('a -> 'c)) -> 'b -> 'c * bool
  method get     : ?guard:('a -> bool) -> unit -> 'a
  method set     : ?guard:('a -> bool) -> 'a -> unit
  method propose : ?guard:('a -> bool) -> 'a -> 'a * bool
  method move    : ?guard:('a -> bool) -> ('a -> 'a) -> 'a * bool
  method async   : <
    set  : ?guard:('a -> bool) -> 'a -> unit;
    move : ?guard:('a -> bool) -> ('a -> 'a) -> unit;
    >
end

class type ['a] private_interface = object
  method private eval    : 'b 'c. ?guard:('a -> bool) -> ('a -> 'b -> 'a * ('a -> 'c)) -> 'b -> 'c * bool
  method private get     : ?guard:('a -> bool) -> unit -> 'a
  method private set     : ?guard:('a -> bool) -> 'a -> unit
  method private propose : ?guard:('a -> bool) -> 'a -> 'a * bool
  method private move    : ?guard:('a -> bool) -> ('a -> 'a) -> 'a * bool
  method private async   : <
    set  : ?guard:('a -> bool) -> 'a -> unit;
    move : ?guard:('a -> bool) -> ('a -> 'a) -> unit;
    >
end

class ['a] to_object_with_public_interface (x:'a t) =
  object
    method eval : 'b 'c. ?guard:('a -> bool) -> ('a -> 'b -> 'a * ('a -> 'c)) -> 'b -> 'c * bool =
      fun ?guard f b -> eval ?guard f b x

    method get : ?guard:('a -> bool) -> unit -> 'a =
      fun ?guard () -> get ?guard x

    method set : ?guard:('a -> bool) -> 'a -> unit =
      fun ?guard v -> set ?guard x v

    method propose : ?guard:('a -> bool) -> 'a -> 'a * bool =
      fun ?guard v -> propose ?guard x v

    method move : ?guard:('a -> bool) -> ('a -> 'a) -> 'a * bool =
      fun ?guard f -> move ?guard x f

    method async =
      object
	method set : ?guard:('a -> bool) -> 'a -> unit =
	  fun ?guard v -> Async.set ?guard x v

	method move : ?guard:('a -> bool) -> ('a -> 'a) -> unit =
	  fun ?guard f -> Async.move ?guard x f
      end
  end

class ['a] to_object_with_private_interface (x:'a t) =
  object
    method private eval : 'b 'c. ?guard:('a -> bool) -> ('a -> 'b -> 'a * ('a -> 'c)) -> 'b -> 'c * bool =
      fun ?guard f b -> eval ?guard f b x

    method private get : ?guard:('a -> bool) -> unit -> 'a =
      fun ?guard () -> get ?guard x

    method private set : ?guard:('a -> bool) -> 'a -> unit =
      fun ?guard v -> set ?guard x v

    method private propose : ?guard:('a -> bool) -> 'a -> 'a * bool =
      fun ?guard v -> propose ?guard x v

    method private move : ?guard:('a -> bool) -> ('a -> 'a) -> 'a * bool =
      fun ?guard f -> move ?guard x f

    method private async =
      object
	method set : ?guard:('a -> bool) -> 'a -> unit =
	  fun ?guard v -> Async.set ?guard x v

	method move : ?guard:('a -> bool) -> ('a -> 'a) -> unit =
	  fun ?guard f -> Async.move ?guard x f
      end
  end

let to_object_with_public_interface  : 'a t -> 'a public_interface =
  fun x -> new to_object_with_public_interface x

let to_object_with_private_interface : 'a t -> 'a private_interface =
  fun x -> new to_object_with_private_interface x


(*let make
 ?mutexes
 ?(equality:('state -> 'state -> bool) option)
 ?(on_proposal:('state -> 'state -> 'state) option)
 ~(get_content:(unit -> 'state))
 ?(set_content:('state -> unit) option)
 ?(propose_content:('state -> 'state * bool) option)
 ()

let bijection : ('a -> 'b) * ('b -> 'a) -> 'a t -> 'b t =
  fun (f,g) t =
    let get_content () = f (t.get_content ()) in
    let propose_content b =
      let (a, flag) = t.propose_content (g b) in
      ((f a), flag)
    in
    let equality b1 =
      let equals_to_b1 = t.equality (g b1) in
      fun b2 -> equals_to_b1 (g b2)
    in

 make ?equality ?on_proposal ~get_content ~set_content ()


let group3 ?on_proposal (x1:'a t) (x2:'b t) (x3:'c t) : ('a * 'b * 'c) t =
  let g23 = group x2 x3 in

let group_array : ('a t array) -> ('a array) t*)

IFDEF DOCUMENTATION_OR_DEBUGGING THEN
module Example = struct

let x = return 42 ;;
let y = return 10 ;;
let z = group_pair x y ;;
eval_propose 5 x ;;

(* x must be odd *)
x.on_proposal#register_thunk
  (fun () _x0 x1 -> if x1 mod 2 = 0 then x1+1 else x1) ;;

(* y must be even *)
y.on_proposal#register_thunk
  (fun () _y0 y1 -> if y1 mod 2 = 0 then y1 else y1+1) ;;

(* y must be the double of x: *)
z.on_proposal#register_thunk
  (fun () (x0,y0) (x1,y1) ->
     if x1<>x0 then (x1, x1*2) else (y1/2, y1)) ;;
(*
get z ;;
 : int * int = (5, 10)

propose 50 x ;;
 : int * bool = (51, true)

get z ;;
 : int * int = (51, 102)

propose 52 x ;;
 : int * bool = (53, true)

get z ;;
 : int * int = (53, 106)

propose 107 y ;;
 : int * bool = (108, true)

get z ;;
 : int * int = (55, 110)
*)

end (* module Example *)
ENDIF