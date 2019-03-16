open Lwt.Infix
open Contents

type 'v contents = 'v Contents.t
exception Empty_queue

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

  module type JOB_QUEUE = sig
    val is_empty: IrminStore.t -> bool Lwt.t
    val push: Job.t -> IrminStore.t -> unit Lwt.t
    val pop: IrminStore.t -> (Job.t, string) result Lwt.t
    val peek_opt: IrminStore.t -> Job.t option Lwt.t
    val peek_tree: IrminStore.t -> (Job.t * IrminStore.tree) option Lwt.t
  end

  module JobQueue: JOB_QUEUE
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

  module type JOB_QUEUE = sig
    val is_empty: B.Store.t -> bool Lwt.t
    val push: Job.t -> B.Store.t -> unit Lwt.t
    val pop: B.Store.t -> (Job.t, string) result Lwt.t
    val peek_opt: B.Store.t -> Job.t option Lwt.t
    val peek_tree: B.Store.t -> (Job.t * B.Store.tree) option Lwt.t
  end

  module IrminStore = B.Store
  module IrminSync = Irmin.Sync(IrminStore)

  module JobQueue = struct
    let of_map m =
      B.Store.find m ["job_queue"]
      >|= fun q -> match q with
      | Some Job_queue js -> js
      | Some _ -> invalid_arg "Can't happen by design"
      | None -> []

    let is_empty m =
      of_map m >|= Job_queue.is_empty

    let push j m = (* TODO: make this atomic *)
      of_map m
      >|= Job_queue.push j
      >>= fun js' -> B.Store.set m
        ~info:(B.make_info ~author:"map" "Add %a to job queue" Job.pp j)
        ["job_queue"]
        (Contents.Job_queue js')
      >|= fun res -> match res with
      | Ok () -> ()
      | Error _ -> invalid_arg "some error"

    (* TODO: make sure the right job is popped *)
    let pop m = (* TODO: make this atomic *)
      of_map m
      >|= Job_queue.pop
      >>= function
      | Ok (j, js) -> 
        B.Store.set m
          ~info:(B.make_info ~author:"map" "Remove %a from job queue" Job.pp j)
          ["job_queue"] (Job_queue js)
        >|= fun res -> (match res with
        | Ok () -> Ok j
        | Error _ -> Error "Store_error")
      | Error e -> Lwt.return @@ Error e

    let peek_opt m =
      of_map m
      >|= Job_queue.peek_opt

    let peek_tree m =
      of_map m
      >>= function
      | [] -> Lwt.return None
      | (h::ts) -> IrminStore.Tree.(add empty ["job_queue"] (Job_queue ts))
        >>= fun tree -> Lwt.return @@ Some (h, tree)
  end

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
