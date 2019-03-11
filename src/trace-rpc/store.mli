
(* A store is an instance of CausalRPC at a particular node *)
type job = string
type job_queue = job list

type 'v contents =
  | Value of 'v
  | Task_queue of Task_queue.t
  | Job_queue of job_queue

module MakeContents (Val: Irmin.Contents.S): Irmin.Contents.S
  with type t = Val.t contents

(* The job queue holds the active jobs to be performed *)
module type JOB_QUEUE = sig
  module Store: Irmin.KV

  module type IMPL = sig
    val job_of_string: string -> job
    val job_to_string: job -> string
    val job_equal: job -> job -> bool

    val is_empty: Store.t -> bool Lwt.t
    val push: job -> Store.t -> unit Lwt.t
    val pop: Store.t -> job Lwt.t
    val peek_opt: Store.t -> job option Lwt.t
  end

  module Impl: IMPL
end

module type S = sig
  module Description: Interface.DESC
  module Value = Description.Val
  module Contents: Irmin.Contents.S with type t = Value.t contents

  module IrminStore: Irmin_git.S
    with type key = string list
     and type step = string
     and type contents = Contents.t
     and type branch = string

  module B: Backend.S
  module IrminSync: Irmin.SYNC with type db = IrminStore.t
  module JobQueue: JOB_QUEUE with module Store = IrminStore
  module Operation: Interface.OPERATION with module Val = Value
end

(** A CausalRPC store. Parameterised on:
    - a CausalRPC backend module
    - an Irmin Git implementation
    - a description of an interface
    - a job_queue format functor
*)
module Make
    (BackendMaker: Backend.MAKER)
    (GitBackend: Irmin_git.G)
    (Desc: Interface.DESC)
    (JQueueMake: functor
       (Val: Irmin.Contents.S)
       (B: Backend.S
        with type Store.key = Irmin.Path.String_list.t
         and type Store.step = string
         and module Store.Key = Irmin.Path.String_list
         and type Store.contents = Val.t contents
         and type Store.branch = string)
       -> (JOB_QUEUE with module Store = B.Store)): S
  with module Description = Desc
   and module Operation = Interface.MakeOperation(Desc.Val)
