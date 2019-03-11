type job = string
type job_queue = job list

let job = Irmin.Type.string
let job_queue = Irmin.Type.list job

type 'v contents =
  | Value of 'v
  | Task_queue of Task_queue.t
  | Job_queue of job_queue

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

module MakeContents (Val: Irmin.Contents.S): Irmin.Contents.S
  with type t = Val.t contents = struct

  type t = Val.t contents

  let t =
    let open Irmin.Type in
    variant "contents" (fun value task_queue branch_name -> function
        | Value v -> value v
        | Task_queue q -> task_queue q
        | Job_queue js -> branch_name js)
    |~ case1 "Value" Val.t (fun v -> Value v)
    |~ case1 "Task_queue" Task_queue.t (fun q -> Task_queue q)
    |~ case1 "Job_queue" job_queue (fun js -> Job_queue js)
    |> sealv

  let merge ~old t1 t2 =

    let open Irmin.Merge.Infix in
    old () >>=* fun old ->

    match (old, t1, t2) with
    | Some Value o, Value a, Value b ->

      (* TODO: work out why optional merge combinators are default in Irmin.Contents.S *)
      (Irmin.Merge.f Val.merge) ~old:(Irmin.Merge.promise (Some o)) (Some a) (Some b)
      >>=* fun x -> (match x with
          | Some x -> Irmin.Merge.ok (Value x)
          | None -> invalid_arg "no value")

    | Some Task_queue o, Task_queue a, Task_queue b ->

      Task_queue.merge ~old:(Irmin.Merge.promise o) a b
      >>=* fun x -> Irmin.Merge.ok (Task_queue x)

    (* Irmin.Merge.conflict "%s" (Format.asprintf "old = %a\n\na = %a\n\nb = %a" pp_task_queue o pp_task_queue a pp_task_queue b) *)

    | _, Job_queue _, Job_queue _ -> Irmin.Merge.conflict "Job_queue"
    | _ -> Irmin.Merge.conflict "Different types in store"

  let merge = Irmin.Merge.(option (v t merge))
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
   and module Operation = Interface.MakeOperation(Desc.Val) = struct

  module Description = Desc
  module Value = Description.Val
  module Contents = MakeContents(Desc.Val)

  module B = BackendMaker(GitBackend)(Contents)
  module IrminStore = B.Store
  module IrminSync = Irmin.Sync(IrminStore)
  module JobQueue = JQueueMake(Desc.Val)(B)
  module Operation = Interface.MakeOperation(Desc.Val)
end