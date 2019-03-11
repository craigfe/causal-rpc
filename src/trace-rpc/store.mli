
(* A store is an instance of CausalRPC at a particular node *)
module type S = sig
  module Description: Interface.DESC
  module Value = Description.Val
  module IrminContents: Irmin.Contents.S with type t = Value.t Contents.t

  module IrminStore: Irmin_git.S
    with type key = string list
     and type step = string
     and type contents = IrminContents.t
     and type branch = string

  module B: Backend.S
  module IrminSync: Irmin.SYNC with type db = IrminStore.t
  module JobQueue: Contents.JOB_QUEUE with module Store = IrminStore
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
         and type Store.contents = Val.t Contents.t
         and type Store.branch = string)
       -> (Contents.JOB_QUEUE with module Store = B.Store)): S
  with module Description = Desc
   and module Operation = Interface.MakeOperation(Desc.Val)
