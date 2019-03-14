open Lwt.Infix
open Contents

type 'v contents = 'v Contents.t

module type S = sig
  module Description: Description.S
  module Value = Description.Val
  module IrminContents: Irmin.Contents.S with type t = Value.t contents

  module IrminStore: Irmin_git.S
    with type key = string list
     and type step = string
     and type contents = IrminContents.t
     and type branch = string

  module B: Backend.S
  module IrminSync: Irmin.SYNC with type db = IrminStore.t
  module JobQueue: Contents.JOB_QUEUE with module Store = IrminStore
  module Operation: Operation.S with module Val = Value

  exception Store_error of IrminStore.write_error
  exception Push_error of IrminSync.push_error

  val upstream: uri:string -> branch:string -> Irmin.remote Lwt.t
  val remove_pending_task: Task.t -> IrminStore.tree -> IrminStore.tree Lwt.t
  val remove_pending_tasks: Task.t list -> IrminStore.tree -> IrminStore.tree Lwt.t
end


module Make
    (BackendMaker: Backend.MAKER)
    (GitBackend: Irmin_git.G)
    (Desc: Description.S): S
  with module Description = Desc
   and module Operation = Operation.Make(Desc.Val)
   and type IrminStore.branch = string = struct

  module Description = Desc
  module Value = Description.Val
  module IrminContents = Contents.Make(Desc.Val)

  module B = BackendMaker(GitBackend)(IrminContents)
  module IrminStore = B.Store
  module IrminSync = Irmin.Sync(IrminStore)
  module JobQueue = Job_queue.Make(Desc.Val)((B))
  module Operation = Operation.Make(Desc.Val)

  exception Store_error of IrminStore.write_error
  exception Push_error of IrminSync.push_error

  let upstream ~uri ~branch =

    (* It's currently necessary to manually case-split between 'local' remotes
       using the file local protocol and the standard git protocol
       https://github.com/mirage/irmin/issues/589 *)

    if String.sub uri 0 7 = "file://" then
      let dir = String.sub uri 7 (String.length uri - 7) in
      Irmin_git.config dir
      |> IrminStore.Repo.v
      >>= fun repo -> IrminStore.of_branch repo branch
      >|= Irmin.remote_store (module IrminStore)
    else
      Lwt.return (B.remote_of_uri uri)

  (* Take a store_tree and remove a pending task from it *)
  let remove_pending_task (task: Task.t) (store_tree: IrminStore.tree): IrminStore.tree Lwt.t =
    IrminStore.Tree.get store_tree ["task_queue"]
    >>= (fun q -> match q with
        | Task_queue tq -> Lwt.return tq
        | _ -> Lwt.fail Exceptions.Internal_type_error)
    >|= (fun (todo, pending) -> Task_queue
            (todo, List.filter (fun t -> t <> task) pending))
    >>= IrminStore.Tree.add store_tree ["task_queue"]

  (* Take a store_tree and remove a list of pending tasks from it *)
  let remove_pending_tasks (tasks: Task.t list) (store_tree: IrminStore.tree): IrminStore.tree Lwt.t =
    Lwt_list.fold_right_s remove_pending_task tasks store_tree
end
