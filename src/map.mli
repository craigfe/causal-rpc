
exception Empty_queue

(** A task is an operation, a list of parameters and a key specifying the value
    on which to perform the operation *)
type task = {
  name: string;
  params: Interface.Param.t list;
  key: string;
}

type ('v, 'jq) contents =
  | Value of 'v
  | Task_queue of (task list * task list)
  | Job_queue of 'jq

module type QUEUE_TYPE = sig
  type t
  type job

  val t: t Irmin.Type.t
  val job: job Irmin.Type.t
end

module type JOB_QUEUE = sig
  type t
  (** The type of job queues *)

  type job
  (** The type of jobs *)

  module Store: Irmin.KV

  module Type: QUEUE_TYPE
    with type t = t
     and type job = job

  module type IMPL = sig
    val job_of_string: string -> job
    val job_to_string: job -> string

    val is_empty: Store.t -> bool Lwt.t
    val push: job -> Store.t -> unit Lwt.t
    val pop: Store.t -> job Lwt.t
    val peek_opt: Store.t -> job option Lwt.t
  end

  module Impl: IMPL
end

module MakeContents (Val: Irmin.Contents.S) (JQueue: QUEUE_TYPE): Irmin.Contents.S
  with type t = (Val.t, JQueue.t) contents

exception Malformed_params of string
module type S = sig
  type key = string
  (** The type of the map keys *)

  type value
  (** The type of the map values *)

  type queue
  (** The type of the job queue *)

  type t
  (** The type of maps from type [key] to type [value] *)

  module Value: Irmin.Contents.S
  module Contents: Irmin.Contents.S with type t = (value, queue) contents
  module Store: Irmin.KV with type contents = Contents.t
  module Sync: Irmin.SYNC with type db = Store.t
  module JobQueue: JOB_QUEUE with module Store = Store
  module Operation: Interface.OPERATION with type value = value

  type 'a operation = 'a Interface.Operation(Value).unboxed
  (** The type of operations to be performed on the map *)

  type 'a params = 'a Interface.Operation(Value).params

  (* -- TESTING PURPOSES --------------------------------- *)
  val task_queue_is_empty: t -> bool
  val job_queue_is_empty: t -> bool
  val generate_task_queue: 'a operation -> 'a params -> t -> (value, queue) contents
  (* ----------------------------------------------------- *)

  val of_store: Sync.db -> t
  (** Return the map corresponding to an underlying store representation *)

  val empty: ?directory:string -> unit -> t
  (** The empty map. *)

  val is_empty: t -> bool
  (** Test whether a map is empty or not. *)

  val mem: key -> t -> bool
  (** [mem x m] returns true iff [m] contains a binding for [x] *)

  val add: key -> value -> t -> t
  (** [add x y m] returns a map containing the same bindings as [m],
      plus a binding of [x] to [y]. If [x] was already bound in [m],
      its previous binding is replaced. *)

  val find: key -> t -> value
  (** [find x m] returns the current binding of [x] in [m],
      or raises [Not_found] if no such binding exists. *)

  val remove: key -> t -> t
  (** [remove x m] returns a map containing the same bindings as [m],
      except for [x] which is unbound in the returned map. *)

  val size: t -> int
  (** Return the number of bindings in the map *)

  val keys: t -> key list
  (** Return a list of keys in the map *)

  val values: t -> value list
  (** Return a list of values in the map *)

  val map: 'a operation -> 'a params -> t -> t
  (** [map m] returns a map with the same domain as [m] in which
      the associated value [a] of all bindings of [m] have been
      replaced by the result of applying _a_ function to [a] *)
end

module Make
    (Desc: Interface.DESC)
    (Kv_maker: Irmin_git.KV_MAKER)
    (QueueType: QUEUE_TYPE)
    (JQueueMake: functor
       (Val: Irmin.Contents.S)
       (St: Irmin.KV with type contents = (Val.t, QueueType.t) contents)
       -> (JOB_QUEUE with module Store = St)
    ): S
  with type value = Desc.S.t
   and type queue = QueueType.t
(** Functor building an implementation of the map structure given:
     - a value for the map to contain
     - a set of operations on that type
     - a queue type
     - a job queue implementation *)
