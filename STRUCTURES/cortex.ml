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

(* Do not remove the following comment: it's an ocamldoc workaround. *)
(** *)

module Mutex = MutexExtra.Recursive 

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
  | Single m -> Mutex.wait alert m
  | Group (frame, ms, ws) ->
      begin
        List.iter Mutex.unlock ws;
        Mutex.wait alert frame;
        List.iter Mutex.lock ms;
      end

  let apply_with_mutex (t:t) f x =
   lock t;
   try
     let result = f x in
     unlock t;
     result
   with e -> begin
     unlock t;
     raise e;
   end
 
  (* let with_mutex (t:t) thunk = apply_with_mutex t thunk () *)
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
   
 end (* module Mutex_group *)

module Open = struct

(* The structure of open cortex: *)
type 'state t = {
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

(* Open.make *)
let make
 ?(equality:('state -> 'state -> bool) option)
 ?(on_proposal:('state -> 'state -> 'state) option)
 ~(get_content:(unit -> 'state))
 ?(set_content:('state -> unit) option)
 ?(propose_content:('state -> 'state * bool) option)
 ()
 : 'state t
 =
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
  { alert_on_commit   = alert_on_commit;
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
 
(* Open.revno_equality *) 
let revno_equality x =
  let r = x.revno in
  (fun x' -> x==x' && x'.revno = r)

(* Open.revno_or_content_equality *) 
let revno_or_content_equality : 'a t -> 'a t -> bool =
  fun x1 ->
    let r = x1.revno in
    fun x2 ->
      (x1==x2 && x2.revno = r) ||
      (let v1 = x1.get_content () in
       let v2 = x2.get_content () in
       (x1.equality v1 v2) && (x2.equality v2 v1))

(* The universal method Open.eval (unprotected and without guards): *)
let eval : 'a 'b. ('state -> 'a -> 'state * ('state -> 'b)) -> 'a -> 'state t -> 'b * bool =
  fun f a t ->
    let current = t.get_content () in
    let equals_to_current = !(t.equals_to_current) in
    let (first_proposal, b_of_state) =
      (* Apply the update-proposal `f' *)
      f current a
    in
    (* This test is useful for thread waiting for a commit of a member
       which is no longer in use. This test is redundant because the thread
       should be stopped by the condition ~membership (see below): *)
    if !(t.no_longer_in_use) then ((b_of_state current), false) else
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

(* The `propose' specific case (useful for grouping): *)
let eval_propose : 'state -> 'state t -> 'state * bool =
  fun s1 t -> eval (fun s0 s1 -> s1, (fun s2 -> s2)) s1 t

(* Mutexes must be provided for waiting. Note that the evaluation starts
   immediately if the guard is verified. *)
let guarded_eval
  : 'a 'b.
       guard:('state -> bool) ->
       mutexes:Mutex_group.t ->
       ('state -> 'a -> 'state * ('state -> 'b)) -> 'a -> 'state t -> 'b * bool
  =
  fun ~guard ~mutexes f a t ->
  if guard (t.get_content ())
    then eval f a t
    else begin
      incr (t.waiting_no);
      Mutex_group.wait (t.alert_on_commit) mutexes;
      while not (guard (t.get_content ())) do
	incr (t.waiting_no);
	Mutex_group.wait (t.alert_on_commit) mutexes
      done;
      eval f a t
    end

exception Membership_failure

(* Mutexes must be provided for waiting. The cortex that commits is not necessarely
   the same that will be evaluated (even if by default is the same), but we suppose
   that the provided mutexes lock *both* cortex. 
   Note also tha we start waiting anyway: the evaluation will be executed after
   at least one commit. *)
let eval_after_commit
  : ?monitored:('member t) -> (* the cortex that we are waiting for *)
    ?membership:(unit -> bool) ->
    ?guard:('state -> bool) ->
     mutexes:Mutex_group.t ->
    ('state -> 'a -> 'state * ('state -> 'b)) -> 
    (* Arguments are flipped for efficiency: *)
    'state t -> 'a -> ('b * bool) option
  =
  fun ?monitored ?(membership=(fun () -> true)) ?guard ~mutexes f t ->
    let (alert_on_commit, waiting_no) = 
      match monitored with 
      | None   -> (t.alert_on_commit, t.waiting_no)
      | Some m -> (m.alert_on_commit, m.waiting_no)
    in
    let eval_without_guard a =
      begin
	(* Start waiting anyway: *)
	incr (waiting_no);
	Mutex_group.wait (alert_on_commit) mutexes;
        (* Eval after a commit (if the membership is still valid): *)
	if membership () then Some (eval f a t) else None
      end
    in
    let eval_with_guard guard a =
      begin
        try
	  (* Start waiting anyway: *)
	  incr (waiting_no);
	  Mutex_group.wait (alert_on_commit) mutexes;
	  (if not (membership ()) then raise Membership_failure);
	  while not (guard (t.get_content ())) do
	    incr (waiting_no);
	    Mutex_group.wait (alert_on_commit) mutexes;
	    (if not (membership ()) then raise Membership_failure);
	  done;
	  (* Eval after at least one commit: *)
	  Some (eval f a t)
	with
	  Membership_failure -> None
      end
    in
    match guard with
    | None       -> eval_without_guard
    | Some guard -> eval_with_guard guard


let repeat_eval_after_commit
  : ?monitored:('member t) -> (* the cortex that we are waiting for *)
    ?membership:(unit -> bool) ->
    ?guard:('state -> bool) ->
    mutexes:Mutex_group.t ->
    ('state -> 'a -> 'state * ('state -> 'b)) -> 
    'state t -> 
    (* the boolean result of `folder' denotes the `break' condition: *)
    folder:('c -> ('b * bool) -> 'c * bool) ->
    'a -> 'c -> 'c
  =
  fun ?monitored ?membership ?guard ~mutexes f t ->
  let eval = eval_after_commit ?monitored ?membership ?guard ~mutexes f t in
  fun ~folder a c ->
  let rec loop c =
    match eval a with
    | None -> c (* A membership failure => break *)
    | Some result -> 
        let (c', break) = folder c result in
        if break then c' else loop c'
  in
  loop c
      
end (* module Open *)

(* Mutexes and related conditions are ordered by mutexes simply with Pervasives.compare.
   The inner evaluation is unprotected. *)
type 'state t = Mutex_group.t * 'state Open.t
(* Recursive cortex?  
type 'a r = ('Mutex_group.t t) * 'a Open.t
*)

(*let copy_but_already_protected_by ~mutexes t =
 (mutexes, snd t)
;;*)

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
 let u = Open.make ?equality ?on_proposal ~get_content ?set_content ?propose_content () in
 (mutexes, u)


(* The universal method (protected and guarded version): *)
let eval 
  : ?guard:('state -> bool) -> 
    ('state -> 'a -> 'state * ('state -> 'b)) -> 'a -> 'state t -> 'b * bool 
  =
  fun ?guard f a (t_mutexes, t) ->
    match guard with
    | None ->
        Mutex_group.apply_with_mutex t_mutexes (Open.eval f a) t
    | Some guard ->
	Mutex_group.apply_with_mutex t_mutexes (Open.guarded_eval ~guard ~mutexes:t_mutexes f a) t

(* Note that in the protected version the arguments are flipped with respect
   to the unprotected one. Warning: the monitored cortex, if provided, must be 
   also locked by the mutexes of t. *)
let eval_after_commit 
  : ?monitored:('c t) ->         (* the cortex that we are waiting for *)
    ?membership:(unit -> bool) ->
    ?guard:('state -> bool) ->
    ('state -> 'a -> 'state * ('state -> 'b)) -> 
    'a -> 'state t -> ('b * bool) option
  = 
  fun ?monitored ?membership ?guard f a t ->
  let monitored = Option.map snd monitored in
  let (mutexes, u) = t in
  Mutex_group.apply_with_mutex mutexes
    (Open.eval_after_commit ?monitored ?membership ?guard ~mutexes f u) a


(* Note that in the protected version the arguments are flipped with respect
   to the unprotected one. Warning: the monitored cortex, if provided, must be 
   also locked by the mutexes of t. *)
let repeat_eval_after_commit 
  : ?monitored:('m t) ->         (* the cortex that we are waiting for *)
    ?membership:(unit -> bool) ->
    ?guard:('state -> bool) ->
    folder:('c -> ('b * bool) -> 'c * bool) ->
    ('state -> 'a -> 'state * ('state -> 'b)) -> 
    'a -> 'state t -> 'c -> 'c
  = 
  fun ?monitored ?membership ?guard ~folder f a t c ->
  let monitored = Option.map snd monitored in
  let (mutexes, u) = t in
  Mutex_group.apply_with_mutex mutexes
    (Open.repeat_eval_after_commit ?monitored ?membership ?guard ~mutexes f u ~folder a) c

    
let eval_get ?guard t = fst (eval ?guard (fun s () -> s, (fun s -> s)) () t)
let eval_set ?guard s1 t = ignore (eval ?guard (fun s0 s1 -> s1, (fun s2 -> s2)) s1 t)
let eval_propose ?guard s1 t = eval ?guard (fun s0 s1 -> s1, (fun s2 -> s2)) s1 t
let eval_move ?guard f t = eval ?guard (fun s0 () -> (f s0), (fun s2 -> s2)) () t

(* Flipped versions: *)
let get ?guard t = fst (eval ?guard (fun s () -> s, (fun s -> s)) () t)
let set ?guard t s1 = ignore (eval ?guard (fun s0 s1 -> s1, (fun s2 -> s2)) s1 t)
let propose ?guard t s1 = eval ?guard (fun s0 s1 -> s1, (fun s2 -> s2)) s1 t
let move ?guard t f = eval ?guard (fun s0 () -> (f s0), (fun s2 -> s2)) () t
let apply ?guard t f = fst (eval ?guard (fun s () -> s, (fun s -> f s)) () t)

let on_proposal_append (t_mutexes, t) thunk =
  Mutex_group.with_mutex t_mutexes
    (fun () ->
       t.Open.on_proposal#register_thunk (fun () -> thunk))

let on_proposal_remove (t_mutexes, t) id =
  Mutex_group.with_mutex t_mutexes
    (fun () ->
       t.Open.on_proposal#remove id)

let on_proposal_clear (t_mutexes, t) =
  Mutex_group.with_mutex t_mutexes
    (fun () ->
       Container.Queue_with_identifiers.clear (t.Open.on_proposal#as_queue))

(* Flipped and asynchronous versions: *)
module Async = struct

  let set ?guard t s1 =
    ignore (Thread.create (fun () -> eval ?guard (fun s0 s1 -> s1, (fun s2 -> s2)) s1 t) ())

  let move ?guard t f =
    ignore (Thread.create (fun () -> eval ?guard (fun s0 () -> (f s0), (fun s2 -> s2)) () t) ())

end (* module Async *)

let return_with_mutexes
 ~mutexes
 ?(equality:('state -> 'state -> bool) option)
 ?(on_proposal:('state -> 'state -> 'state) option)
 (content:'state)
 =
 let cell = ref content in
 let get_content () = !cell in
 let set_content v = (cell := v) in
 make ~mutexes ?equality ?on_proposal ~get_content ~set_content ()

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



(*let duplicate ?on_proposal (member : 'a t) : ('a t) t =
  let (member_mutexes, member_u) = member in
  let mutexes = ref !member_mutexes in
  let equality = revno_equality in
  Mutex_group.with_mutex x_mutexes 
    (fun () -> 
      let content_copy = ref member in
      let get_content () = !content_copy in
      let proposal = get_content in
      let set_content y = (content_copy := y; mutexes := !(fst y)) in
      let result = make ~mutexes ~equality ?on_proposal ~get_content ~set_content () in
      let trigger_on = make_trigger_on_member_commit ~proposal ~group:result in
      let _thd1 = Thread.create (trigger_on) x in
      result)*)

(*
let connection (f:'a -> 'b) (g: 'b -> 'a) (t: 'a t) : 'b t = 
 {
 alert_on_commit    = t.alert_on_commit;
 get_content        = fun () -> f (t.get_content ());
 propose_content    = fun b -> let (a,flag) = t.propose_content (g b) in ((f a), flag);
 revno              = t.revno;
 waiting_no         = t.waiting_no;
 equality           = fun b -> let equals_b = t.equality (g b) in fun b' -> equals_b (g b');
 on_proposal        : ('state -> 'state -> 'state) Thunk.fifo_container;
 (* "Private methods": *)
 on_proposal_container_unchanged : unit -> bool;
 no_longer_in_use   : bool ref;
 equals_to_current  : ('state -> bool) ref;
 }
;;
*)      
      

  
let repeat_move_proposal_to_group_on_member_commit 
  : ?membership:(unit -> bool) ->
    ?guard:('state -> bool) -> 
    ?action_and_break_decision_when_accepted:('state -> 'state -> 'state -> bool) ->
    move_proposal:('state -> 'state) -> 
    group:'state t -> 
   'member t -> unit 
  =
  fun 
    ?membership
    ?guard 
    ?(action_and_break_decision_when_accepted=fun _ _ _ -> false) 
    ~move_proposal 
    ~group 
    member 
  ->
  let monitored = member in
  (* The method is similar to `move' but the result is the triple
     of states (before_transition, proposed, after_transition): *)
  let mthd s0 () =
    let s1 = move_proposal s0 in
    (s1, (fun s2 -> (s0,s1,s2))) 
  in
  let folder () ((s0,s1,s2), changed) = 
    let break = 
      if changed 
       then (action_and_break_decision_when_accepted s0 s1 s2) 
       else false
    in
    ((), break) 
  in
  repeat_eval_after_commit 
    ~monitored ?membership ?guard ~folder mthd () group ()

let repeat_propose_to_group_on_member_commit 
 ?membership ?guard ?action_and_break_decision_when_accepted
 ~proposal ~group member 
 = repeat_move_proposal_to_group_on_member_commit 
     ?membership ?guard ?action_and_break_decision_when_accepted
     ~move_proposal:(fun _ -> proposal ()) ~group member
  
let eval_propose ?guard s1 t = 
  eval ?guard (fun s0 s1 -> s1, (fun s2 -> s2)) s1 t
  
let group_single ?on_proposal (member_x : 'a t) : 'a t =
  let (x_mutexes, x) = member_x in
  let mutexes = x_mutexes in
  let equality = x.Open.equality in
  let proposal = x.Open.get_content in
  Mutex_group.with_mutex mutexes 
    (fun () -> 
      let content_copy = ref (proposal ()) in
      let get_content () = !content_copy in
      let rec propose_content v =
	begin
	  content_copy := v;
	  let (v', b) = Open.eval_propose v x in
	  if b
	    then Open.eval_propose v' (snd (Lazy.force result))
	    else (v, false)
	end
      and
	result : ('a t) Lazy.t =
	  lazy (make ~mutexes ~equality ?on_proposal ~get_content ~propose_content ())
      in
      (* end of recursive definition *)
      let (_, group_u) as group = (Lazy.force result) in
      let membership () = not !(group_u.Open.no_longer_in_use) in
      let trigger_on (member) = repeat_propose_to_group_on_member_commit ~membership ~proposal ~group (member) in
      let _thd1 = Thread.create (trigger_on) (member_x) in
      group)

(* La differenza con una view è che gli on_proposal sono distinti. *)
let connection ?on_proposal (f:'a->'b) (g:'b -> 'a) (member_x : 'a t) : 'b t =
  let (x_mutexes, x) = member_x in
  let mutexes = x_mutexes in
  let equality b = 
    let equals_b = x.Open.equality (g b) in 
    fun b' -> equals_b (g b')
  in
  let proposal () = f (x.Open.get_content ()) in
  Mutex_group.with_mutex mutexes 
    (fun () -> 
      let content_copy = ref (proposal ()) in
      let get_content () = !content_copy in
      let rec propose_content v =
	begin
	  content_copy := v;
	  let (v', b) = Open.eval_propose (g v) x in
	  if b
	    then Open.eval_propose (f v') (snd (Lazy.force result))
	    else (v, false)
	end
      and
	result : ('b t) Lazy.t =
	  lazy (make ~mutexes ~equality ?on_proposal ~get_content ~propose_content ())
      in
      (* end of recursive definition *)
      let (_, group_u) as group = (Lazy.force result) in
      let membership () = not !(group_u.Open.no_longer_in_use) in
      let trigger_on (member) = repeat_propose_to_group_on_member_commit ~membership ~proposal ~group (member) in
      let _thd1 = Thread.create (trigger_on) (member_x) in
      group)

      
let lift_equality_to_option : ('a -> 'b -> bool) -> ('a option -> 'b option -> bool) =
fun p ->
 function
 | None   -> ((=)None)
 | Some a -> (function None -> false | Some b -> (p a b))

(* Note that the ~proposal may act on the member which has the same mutexes of the group 
   => mutexes must be recursive! *) 
let lifes 
  ?on_proposal 
  ~(creator : ?previous:'a -> unit -> 'a Open.t) 
  ~(terminal : 'a -> bool) 
  ()
  : ('a option * 'a t) t 
  =
  let equality (ao, at) =
    let au = snd at in
    let equals_to_ao = lift_equality_to_option (au.Open.equality) (ao) in
    let equals_to_at = 
      let p = Open.revno_equality au in
      fun at' -> p (snd at')
    in 
    fun (ao', at') -> (equals_to_ao ao') && (equals_to_at at')
  in
  let mutexes = Mutex_group.single () in
  let new_member ?previous () = 
    let member_u = creator ?previous () in
    (mutexes, member_u) (* same mutexes of the group *)
  in
  let member = new_member () in
  let (_, group_u) as group = 
    return_with_mutexes ~mutexes ~equality ?on_proposal (None, member) 
  in
  let membership_of member () =
    let (_, member') = group_u.Open.get_content () in
    member' == member
  in
  let rec 
    (* A new member is proposed when the previous reaches a terminal state: *)
    guard (_, member) = 
      let (member_m, member_u) = member in
      let member_state = member_u.Open.get_content () in
      terminal (member_state) 
   and
    move_proposal (_, member) = 
      let old_member_state = (snd member).Open.get_content () in
      let member' = new_member ~previous:old_member_state () in
      ((Some old_member_state), member')
   and 
    (* The current thread must be stopped if we are working now on another member.
       These parameter is redundant because of the usage of ~membership: *)
    action_and_break_decision_when_accepted s0 s1 s2 = 
      (* s0, s1, s2  =  before, proposed, after *)
      let break = 
        let (_, member0),(_, member2) = s0, s2 in
        member0 != member2
      in 
      break
   and
    trigger_on (member) = 
      repeat_move_proposal_to_group_on_member_commit 
        ~membership:(membership_of member)
        ~guard
        ~action_and_break_decision_when_accepted
        ~move_proposal
        ~group
        (member)

  in (* end of recursive definition *)
  (* Now we start the first thread monitoring the current member: *)
  let _thd1 = Thread.create (trigger_on) member in
  (* And we define a callback preventing to set the group with a member already terminated.
     The `set' operation applied to a group could be very unsafe if we are not sure that  
     the mutexes of the member are contained in the mutexes of the group. We prevent 
     this problem using exclusively the `creator' function to build members. *)
  let _thunk_id = 
    let mutexes_and_members_are_the_same (_, member0) (_, member1) : bool * bool =
      let (member0_mutexes, member0_open) = member0 in
      let (member1_mutexes, member1_open) = member1 in
      (member0_mutexes = member1_mutexes), (member0_open == member1_open)
    in      
    on_proposal_append (group) 
    (fun s0 s1 -> 
       let s2 = if guard s1 then move_proposal s1 else s1 in
       let same_mutexes, same_members = mutexes_and_members_are_the_same s0 s2 in
       (* Proposal with distinct mutexes are forbidden: *)
       if not (same_mutexes) then s0 else 
       (* Start a monitoring thread if a new member replace the previous: *)
       let () = 
         if same_members then () 
         else begin
           let member' = snd s2 in
           let _thd = Thread.create (trigger_on) member' in
           ()
           end
       in
       s2) 
  in
  group
  
  
let group_pair ?on_proposal (member_x : 'a t) (member_y : 'b t) : ('a * 'b) t =
  let (x_mutexes, x) = member_x in
  let (y_mutexes, y) = member_y in
  let mutexes = Mutex_group.group x_mutexes y_mutexes in
  let equality (a,b) =
    let equals_a = (x.Open.equality a) in
    let equals_b = (y.Open.equality b) in
    fun (a',b') -> (equals_a a') && (equals_b b')
  in
  let proposal () = (x.Open.get_content (), y.Open.get_content ()) in
  Mutex_group.with_mutex mutexes 
    (fun () -> 
      let content_copy = ref (proposal ()) in
      let get_content () = !content_copy in
      let rec propose_content (v1,v2) =
	begin
	  content_copy := (v1,v2);
	  let (v1', b1) = Open.eval_propose v1 x in
	  let (v2', b2) = Open.eval_propose v2 y in
	  if b1 || b2
	    then Open.eval_propose (v1',v2') (snd (Lazy.force result))
	    else ((v1,v2), false)
	end
      and
	result = lazy (make ~mutexes ~equality ?on_proposal ~get_content ~propose_content ())
      in
      let (_, group_u) as group = (Lazy.force result) in
      let membership () = not !(group_u.Open.no_longer_in_use) in
      let trigger_on (member) = repeat_propose_to_group_on_member_commit ~membership ~proposal ~group (member) in
      let _thd1 = Thread.create (trigger_on) (member_x) in
      let _thd2 = Thread.create (trigger_on) (member_y) in
      group)


let ungroup : 'a t -> unit =
  fun (t_mutexes, t) ->
    Mutex_group.with_mutex t_mutexes 
      (fun () -> 
         t.Open.no_longer_in_use := true;
         Container.Queue_with_identifiers.clear (t.Open.on_proposal#as_queue))

     
let group_triple ?on_proposal (member_x1:'a t) (member_x2:'b t) (member_x3:'c t) : ('a * 'b * 'c) t =
  let (x1_mutexes, x1) = member_x1 in
  let (x2_mutexes, x2) = member_x2 in
  let (x3_mutexes, x3) = member_x3 in
  let mutexes = Mutex_group.group_from_list1 (x1_mutexes,[x2_mutexes; x3_mutexes]) in
  let equality (a,b,c) =
    let equals_a = (x1.Open.equality a) in
    let equals_b = (x2.Open.equality b) in
    let equals_c = (x3.Open.equality c) in
    fun (a',b',c') -> (equals_a a') && (equals_b b') && (equals_c c')
  in
  let proposal () =
    (x1.Open.get_content (), x2.Open.get_content (), x3.Open.get_content ())
  in
  Mutex_group.with_mutex mutexes 
    (fun () -> 
      let content_copy = ref (proposal ()) in
      let get_content () = !content_copy in
      let rec propose_content (v1,v2,v3) =
	begin
	  content_copy := (v1,v2,v3);
	  let (v1', b1) = Open.eval_propose v1 x1 in
	  let (v2', b2) = Open.eval_propose v2 x2 in
	  let (v3', b3) = Open.eval_propose v3 x3 in
	  if b1 || b2 || b3
	    then Open.eval_propose (v1',v2', v3') (snd (Lazy.force result))
	    else ((v1,v2,v3), false)
	end
      and
	result = lazy (make ~mutexes ~equality ?on_proposal ~get_content ~propose_content ())
      in
      let (_, group_u) as group = (Lazy.force result) in
      let membership () = not !(group_u.Open.no_longer_in_use) in
      let trigger_on (member) = repeat_propose_to_group_on_member_commit ~membership ~proposal ~group (member) in
      let _thd1 = Thread.create (trigger_on) (member_x1) in
      let _thd2 = Thread.create (trigger_on) (member_x2) in
      let _thd3 = Thread.create (trigger_on) (member_x3) in
      group)


let group_quadruple ?on_proposal
  (member_x1:'a t) (member_x2:'b t) (member_x3:'c t) (member_x4:'d t)
  : ('a * 'b * 'c * 'd) t
  =
  let (x1_mutexes, x1) = member_x1 in
  let (x2_mutexes, x2) = member_x2 in
  let (x3_mutexes, x3) = member_x3 in
  let (x4_mutexes, x4) = member_x4 in
  let mutexes = Mutex_group.group_from_list1 (x1_mutexes,[x2_mutexes; x3_mutexes; x4_mutexes]) in
  let equality (a,b,c,d) =
    let equals_a = (x1.Open.equality a) in
    let equals_b = (x2.Open.equality b) in
    let equals_c = (x3.Open.equality c) in
    let equals_d = (x4.Open.equality d) in
    fun (a',b',c',d') -> (equals_a a') && (equals_b b') && (equals_c c') && (equals_d d')
  in
  let proposal () =
    (x1.Open.get_content (), x2.Open.get_content (), x3.Open.get_content (), x4.Open.get_content ())
  in
  Mutex_group.with_mutex mutexes 
    (fun () -> 
      let content_copy = ref (proposal ()) in
      let get_content () = !content_copy in
      let rec propose_content (v1,v2,v3,v4) =
	begin
	  content_copy := (v1,v2,v3,v4);
	  let (v1', b1) = Open.eval_propose v1 x1 in
	  let (v2', b2) = Open.eval_propose v2 x2 in
	  let (v3', b3) = Open.eval_propose v3 x3 in
	  let (v4', b4) = Open.eval_propose v4 x4 in
	  if b1 || b2 || b3 || b4
	    then Open.eval_propose (v1',v2',v3',v4') (snd (Lazy.force result))
	    else ((v1,v2,v3,v4), false)
	end
      and
	result = lazy (make ~mutexes ~equality ?on_proposal ~get_content ~propose_content ())
      in
      let (_, group_u) as group = (Lazy.force result) in
      let membership () = not !(group_u.Open.no_longer_in_use) in
      let trigger_on (member) = repeat_propose_to_group_on_member_commit ~membership ~proposal ~group (member) in
      let _thd1 = Thread.create (trigger_on) (member_x1) in
      let _thd2 = Thread.create (trigger_on) (member_x2) in
      let _thd3 = Thread.create (trigger_on) (member_x3) in
      let _thd4 = Thread.create (trigger_on) (member_x4) in
      group)


let group_quintuple ?on_proposal
  (member_x1:'a t) (member_x2:'b t) (member_x3:'c t) (member_x4:'d t) (member_x5:'e t)
  : ('a * 'b * 'c * 'd * 'e) t
  =
  let (x1_mutexes, x1) = member_x1 in
  let (x2_mutexes, x2) = member_x2 in
  let (x3_mutexes, x3) = member_x3 in
  let (x4_mutexes, x4) = member_x4 in
  let (x5_mutexes, x5) = member_x5 in
  let mutexes =
    Mutex_group.group_from_list1 (x1_mutexes,[x2_mutexes; x3_mutexes; x4_mutexes; x5_mutexes])
  in
  let equality (a,b,c,d,e) =
    let equals_a = (x1.Open.equality a) in
    let equals_b = (x2.Open.equality b) in
    let equals_c = (x3.Open.equality c) in
    let equals_d = (x4.Open.equality d) in
    let equals_e = (x5.Open.equality e) in
    fun (a',b',c',d',e') -> (equals_a a') && (equals_b b') && (equals_c c') && (equals_d d') && (equals_e e')
  in
  let proposal () =
    (x1.Open.get_content (), x2.Open.get_content (), x3.Open.get_content (),
     x4.Open.get_content (), x5.Open.get_content ())
  in
  Mutex_group.with_mutex mutexes 
    (fun () -> 
      let content_copy = ref (proposal ()) in
      let get_content () = !content_copy in
      let rec propose_content (v1,v2,v3,v4,v5) =
	begin
	  content_copy := (v1,v2,v3,v4,v5);
	  let (v1', b1) = Open.eval_propose v1 x1 in
	  let (v2', b2) = Open.eval_propose v2 x2 in
	  let (v3', b3) = Open.eval_propose v3 x3 in
	  let (v4', b4) = Open.eval_propose v4 x4 in
	  let (v5', b5) = Open.eval_propose v5 x5 in
	  if b1 || b2 || b3 || b4 || b5
	    then Open.eval_propose (v1',v2',v3',v4',v5') (snd (Lazy.force result))
	    else ((v1,v2,v3,v4,v5), false)
	end
      and
	result = lazy (make ~mutexes ~equality ?on_proposal ~get_content ~propose_content ())
      in
      let (_, group_u) as group = (Lazy.force result) in
      let membership () = not !(group_u.Open.no_longer_in_use) in
      let trigger_on (member) = repeat_propose_to_group_on_member_commit ~membership ~proposal ~group (member) in
      let _thd1 = Thread.create (trigger_on) (member_x1) in
      let _thd2 = Thread.create (trigger_on) (member_x2) in
      let _thd3 = Thread.create (trigger_on) (member_x3) in
      let _thd4 = Thread.create (trigger_on) (member_x4) in
      let _thd5 = Thread.create (trigger_on) (member_x5) in
      group)



let group_array ?on_proposal (members : ('a t) array) : ('a array) t =
  let size = Array.length members in
  if size = 0 then invalid_arg "Cortex.group_array: empty array" else
  let member_list = Array.to_list members in
  let (mutex_list, xs) =
    let (ms, xs) = List.split member_list in
    (ms, Array.of_list xs)
  in
  let mutexes =
    let head = List.hd (mutex_list) in
    let tail = List.tl (mutex_list) in
    Mutex_group.group_from_list1 (head, tail)
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
    let equals_v = Array.mapi (fun i v -> xs.(i).Open.equality v) vs in
    and_foldi (fun i v' -> equals_v.(i) v')
  in
  let proposal () = Array.map (fun x -> x.Open.get_content ()) xs in
  Mutex_group.with_mutex mutexes 
    (fun () -> 
      let content_copy = ref (proposal ()) in
      let get_content () = !content_copy in
      let rec propose_content vs =
	begin
	  content_copy := vs;
	  let v'bs = Array.mapi (fun i v -> Open.eval_propose v xs.(i)) vs in
	  let someone_changed = or_foldi (fun i (v',b) -> b) v'bs in
	  if someone_changed
	    then Open.eval_propose (Array.map fst v'bs) (snd (Lazy.force result))
	    else (vs, false)
	end
      and
	result = lazy (make ~mutexes ~equality ?on_proposal ~get_content ~propose_content ())
      in
      let (_, group_u) as group = (Lazy.force result) in
      let membership () = not !(group_u.Open.no_longer_in_use) in
      let trigger_on (member) = repeat_propose_to_group_on_member_commit ~membership ~proposal ~group (member) in
      let _threads = Array.map (fun x -> Thread.create (trigger_on) x) members in
      group)


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
module Example1 = struct

let x = return 42 ;;
let y = return 10 ;;
let z = group_pair x y ;;
eval_propose 5 x ;;

(* x must be odd *)
on_proposal_append x 
  (fun _x0 x1 -> if x1 mod 2 = 0 then x1+1 else x1) ;;

(* y must be even *)
on_proposal_append y
  (fun _y0 y1 -> if y1 mod 2 = 0 then y1 else y1+1) ;;

(* y must be the double of x: *)
on_proposal_append z
  (fun (x0,y0) (x1,y1) ->
     if x1<>x0 then (x1, x1*2) else (y1/2, y1)) ;;
(*
get z ;;
 : int * int = (5, 10)

propose x 50 ;;
 : int * bool = (51, true)

get z ;;
 : int * int = (51, 102)

propose x 52 ;;
 : int * bool = (53, true)

get z ;;
 : int * int = (53, 106)

propose y 107 ;;
 : int * bool = (108, true)

get z ;;
 : int * int = (55, 110)
*)

end (* module Example1 *)

module Example2 = struct

let x = lifes ~creator:(fun ?previous () -> Open.return 42) ~terminal:((>=)0) () ;;
(* val x : (int option * int Cortex.t) Cortex.t = <abstr> *)

let look x = get (snd (get x)) ;;
(* val look : ('a * 'b Cortex.t) Cortex.t -> 'b = <fun> *)

let member x = snd (get x) ;;
(* val member : ('a * 'b Cortex.t) Cortex.t -> 'b Cortex.t = <fun> *)

let y = member x ;;
(* val y : int Cortex.t = <abstr> *)

get y ;;
(* - : int = 42 *)

set y 10;; 
(* - : unit = () *)

look x ;;
(* - : int = 10 *)

set y 20;;
(* - : unit = () *)

look x ;; 
(* - : int = 20 *)

set y 0;;
(* - : unit = () *)

look x ;;
(* - : int = 42 *)

get x ;;
(* - : int option * int Cortex.t = (Some 0, <abstr>) *)

set y (-11);;
(* - : unit = () *)

get x ;;
(* - : int option * int Cortex.t = (Some 0, <abstr>) *)

look x ;;
(* - : int = 42 *)

let z = return 33 ;;
(* val z : int Cortex.t = <abstr> *)

propose x (None, z) ;;
(* - : (int option * int Cortex.t) * bool = ((Some 0, <abstr>), false) *)

propose x (None, y) ;;
(* - : (int option * int Cortex.t) * bool = ((Some (-11), <abstr>), true) *)

look x;;
(* - : int = 42 *)

set y 11 ;;
(* - : unit = () *)

propose x (None, y) ;;
(* - : (int option * int Cortex.t) * bool = ((None, <abstr>), true) *)

look x;;
(* - : int = 11 *)

set y (-11) ;;

look x;;
(* - : int = 42 *)
end
ENDIF