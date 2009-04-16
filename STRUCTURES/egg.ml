(** Values released by a writer once forever, acting as synchronization barriers. *)

type 'a t = {
  mutable barrier   : bool        ;
  condition         : Condition.t ;
  mutex             : Mutex.t     ;
  mutable egg       : 'a option   ;
  }

(** Create a barrier, i.e. an egg t. *)
let create () = {
  barrier         = true ;
  condition       = Condition.create () ;
  mutex           = Mutex.create () ;
  egg             = None ;
  }

(* Included here from MutexExtra for efficiency. *)
let with_mutex mutex thunk =
  Mutex.lock mutex;
  try
    let result = thunk () in
    Mutex.unlock mutex;
    result
  with e -> begin
    Mutex.unlock mutex;
    (Printf.eprintf
      "Semaphore.with_mutex: exception %s raised in critical section. Unlocking and re-raising.\n"
      (Printexc.to_string e));
    raise e;
  end

(** Wait for the egg. If the egg is ready, return immediately. *)
let wait t =
  with_mutex t.mutex (fun () ->
    begin
     while t.barrier do
       (Condition.wait t.condition t.mutex)
     done;
     match t.egg with
     | Some x -> x
     | None   -> assert false
    end)

(** Release the egg once forever. Broadcast all pending readers. Future readers will get the egg immediately without waiting. *)
let release t v =
  with_mutex t.mutex (fun () ->
    begin
     (t.barrier <- false);
     (t.egg <- Some v);
     (Condition.broadcast t.condition);
    end)

(** Look at the egg status. *)
let status t =
  with_mutex t.mutex (fun () -> t.egg)
