open Lwt.Infix

exception Empty_queue

type task = {
  name: string;
  params: Type.Boxed.t list;
  key: string;
}

let task =
  let open Irmin.Type in
  record "task" (fun name params key -> { name; params; key })
  |+ field "name" string (fun t -> t.name)
  |+ field "params" (list Type.Boxed.irmin_t) (fun t -> t.params)
  |+ field "key" string (fun t -> t.key)
  |> sealr

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

module MakeContents (Val: Irmin.Contents.S) (JQueueType: QUEUE_TYPE): Irmin.Contents.S
  with type t = (Val.t, JQueueType.t) contents = struct

  type t = (Val.t, JQueueType.t) contents

  let t =
    let open Irmin.Type in
    variant "contents" (fun value task_queue branch_name -> function
        | Value v -> value v
        | Task_queue q -> task_queue q
        | Job_queue js -> branch_name js)
    |~ case1 "Value" Val.t (fun v -> Value v)
    |~ case1 "Task_queue" (pair (list task) (list task)) (fun q -> Task_queue q)
    |~ case1 "Job_queue" JQueueType.t (fun js -> Job_queue js)
    |> sealv

  let merge = Irmin.Merge.(option (default t))
end

exception Malformed_params of string
exception Timeout

module type S = sig

  module Value: Irmin.Contents.S

  type key = string
  type queue

  type t

  module Contents: Irmin.Contents.S with type t = (Value.t, queue) contents
  module Store: Irmin_unix.Git.S
    with type key = Irmin.Path.String_list.t
     and type step = string
     and module Key = Irmin.Path.String_list
     and type contents = Contents.t
     and type branch = string
     and module Git = Irmin_unix.Git.FS.G
  module Sync: Irmin.SYNC with type db = Store.t
  module JobQueue: JOB_QUEUE with module Store = Store
  module Operation: Interface.OPERATION with module Val = Value

  exception Store_error of Store.write_error
  type 'a params = 'a Interface.MakeOperation(Value).params

  (* Here for testing purposes *)
  val task_queue_is_empty: t -> bool
  val job_queue_is_empty: t -> bool
  val generate_task_queue: 'a Operation.Unboxed.t -> 'a params -> t -> (Value.t, queue) contents
  (* ------------------------- *)

  val of_store: Sync.db -> t
  val to_store: t -> Sync.db

  val empty: ?directory:string -> unit -> t
  val is_empty: t -> bool
  val mem: key -> t -> bool
  val add: ?message:string -> key -> Value.t -> t -> t
  val find: key -> t -> Value.t
  val remove: key -> t -> t
  val size: t -> int
  val keys: t -> key list
  val values: t -> Value.t list
  val map: ?timeout:float -> 'a Operation.Unboxed.t -> 'a params -> t -> t Lwt.t
end

module Make
    (Desc: Interface.DESC)
    (QueueType: QUEUE_TYPE)
    (JQueueMake: functor
       (Val: Irmin.Contents.S)
       (St: Irmin_unix.Git.S
        with type key = Irmin.Path.String_list.t
         and type step = string
         and module Key = Irmin.Path.String_list
         and type contents = (Val.t, QueueType.t) contents
         and type branch = string
         and module Git = Irmin_unix.Git.FS.G)
       -> (JOB_QUEUE with module Store = St)
    ): S
  with module Value = Desc.Val
   and module Operation = Interface.MakeOperation(Desc.Val)
   and type queue = QueueType.t = struct

  module Value = Desc.Val
  module Contents = MakeContents(Desc.Val)(QueueType)
  module Store = Irmin_unix.Git.FS.KV(Contents)
  module Sync = Irmin.Sync(Store)
  module JobQueue = JQueueMake(Desc.Val)(Store)
  module Operation = Interface.MakeOperation(Desc.Val)

  type key = string
  type value = Value.t
  type queue = QueueType.t
  type 'a params = 'a Operation.params

  type t = Sync.db
  (* A map is a branch in an Irmin Key-value store *)

  exception Store_error of Store.write_error

  let generate_random_directory () =
    Misc.generate_rand_string ~length:20 ()
    |> Pervasives.(^) "/tmp/irmin/set/"
    |> fun x -> Logs.info (fun m -> m "No directory supplied. Generated random directory %s" x); x

  let empty ?(directory=generate_random_directory()) () =
    let config = Irmin_git.config ~bare:true directory in

    if String.sub directory 0 11 <> "/tmp/irmin/"
       then invalid_arg ("Supplied directory (" ^ directory ^ ") must be in /tmp/irmin/");

    (* Delete the directory if it already exists... Unsafe! *)
    let ret_code = Sys.command ("rm -rf " ^ directory) in begin
      if (ret_code <> 0) then invalid_arg "Unable to delete directory";

      let lwt = Store.Repo.v config
        >>= fun repo -> Store.of_branch repo "master"
      in Lwt_main.run lwt
    end

  let of_store s = s
  let to_store s = s

  let mem key m =
    let lwt =
      Store.tree m
      >>= fun tree -> Store.Tree.list tree ["vals"]
      >|= List.exists (fun (x,_) -> x = key)
    in Lwt_main.run lwt

  let add ?message key value m =
    let message = (match message with
        | Some m -> m
        | None -> Printf.sprintf "Committing to key %s" key) in

    let lwt =
      Store.set m
        ~info:(Irmin_unix.info ~author:"client" "%s" message)
        ["vals"; key]
        (Value value)
      >|= fun res -> match res with
      | Ok () -> ()
      | Error _ -> invalid_arg "some error"

    in Lwt_main.run lwt; m

  let find key m =
    let lwt =
      (* Get the value from the store and deserialise it *)
      Store.get m ["vals"; key]
      >|= fun value -> match value with
      | Value v -> v
      | _ -> invalid_arg "Can't happen by design"

    in try
      Lwt_main.run lwt
    with Invalid_argument _ -> raise Not_found

  let remove _ _ = invalid_arg "TODO"

  let size m =
    let lwt =
      Store.tree m
      >>= fun tree -> Store.Tree.list tree ["vals"]
      (* >|= List.filter (fun (_, typ) -> typ = `Contents) *)
      >|= List.length
    in Lwt_main.run lwt

  let is_empty m =
    (size m) == 0

  let keys m =
    let lwt =
      Store.tree m
      >>= fun tree -> Store.Tree.list tree ["vals"]
      >|= List.map(fst)
    in Lwt_main.run lwt

  let values m =
    let lwt =
      Store.tree m
      >>= fun tree -> Store.Tree.list tree ["vals"]
      >>= Lwt_list.map_p (fun (x, _) -> Store.get m ["vals"; x])
      >|= List.map (fun value -> match value with
          | Value v -> v
          | _ -> invalid_arg "Can't happen by design"
        )
    in Lwt_main.run lwt

  let get_task_queue m =
    Store.find m ["task_queue"]
    >|= fun q -> match q with
    | Some Task_queue tq -> tq
    | Some _ -> invalid_arg "Can't happen by design"
    | None -> ([], [])

  let task_queue_is_empty branch =
    let lwt =
      get_task_queue branch
      >|= fun q -> match q with
      | ([], []) -> true
      | _ -> false
    in Lwt_main.run lwt

  let task_queue_size branch =
    let lwt =
      get_task_queue branch
      >|= fun (a, b) -> (List.length a) + (List.length b)
    in Lwt_main.run lwt

  let job_queue_is_empty m =
    Lwt_main.run (JobQueue.Impl.is_empty m)

  let rec flatten_params: type a. a params -> Type.Boxed.t list = fun ps ->
    match ps with
    | Interface.Unit -> []
    | Interface.Param (typ, p, ps) -> ((Type.Boxed.box typ p)::flatten_params(ps))

  let generate_task_queue: type a. a Operation.Unboxed.t -> a params -> t -> (value, queue) contents = fun operation params map ->
    let name = Operation.Unboxed.name operation in
    let param_list = flatten_params params in

    keys map
    |> List.map (fun key -> {name; params = param_list; key})
    |> (fun ops ->
        Logs.app (fun m -> m "Generated task queue of [%s]"
                     (List.map (fun {name = n; params = _; key = k} ->
                          Printf.sprintf "{name: %s; key: %s}" n k) ops
                      |> String.concat ", "));
        ops)
    |> fun ops -> Task_queue (ops, []) (* Initially there are no pending operations *)

  let set_task_queue q m =
    Store.set ~info:(Irmin_unix.info ~author:"map" "Specifying workload")
      m ["task_queue"] q

    >|= fun res -> match res with
    | Ok () -> ()
    | Error we -> raise (Store_error we)

  let map: type a. ?timeout:float -> a Operation.Unboxed.t -> a params -> t -> t Lwt.t =
    fun ?(timeout=5.0) operation params m ->

    (* Generate a new unique branch name for the map *)
    let rec unique_name_gen () =
      Lwt.wrap (fun () -> "map--" ^ Misc.generate_rand_string ~length:8 ())
      >>= fun map_name -> Store.Branch.mem (Store.repo m) map_name
      >>= fun exists -> if exists then unique_name_gen () else Lwt.return map_name
    in unique_name_gen ()

    >>= fun map_name -> Logs_lwt.app (fun m -> m "Map operation issued. Branch name %s" map_name)

    (* Push the job to the job queue *)
    >>= fun () -> JobQueue.Impl.push (JobQueue.Impl.job_of_string map_name) m

    (* Create a new branch to isolate the operation *)
    >>= fun () -> Store.clone ~src:m ~dst:map_name
    >>= fun branch -> Store.merge_with_branch m
      ~info:(Irmin_unix.info ~author:"map" "Merged") "master"
    >|= (fun merge -> match merge with
        | Ok () -> ()
        | Error _ -> invalid_arg "merge conflict")

    (* Generate and commit the task queue *)
    >>= fun () -> set_task_queue (generate_task_queue operation params m) branch

    (* Wait for the task queue to be empty *)
    >>= fun () ->

    let inactivity_count = ref 0.0 in
    let reset_count diff = match diff with
      | `Added _ -> (inactivity_count := 0.0; Lwt.return_unit)
      | _ -> Lwt.return_unit in

    Store.watch branch reset_count

    >>= fun watch -> Logs_lwt.app (fun m -> m "Waiting for the task queue to be empty, with timeout %f" timeout)
    >>= fun () ->
    let rec inner () =

      let sleep_interval = Pervasives.min (timeout /. 8.0) 1.0 in

      if task_queue_is_empty branch then (* we are done *)
        Store.unwatch watch

      else if !inactivity_count >= timeout then (* we have been waiting for too long *)
        Logs_lwt.app (fun m -> m "Inactivity count: %f" (!inactivity_count))
        >>= fun () -> Lwt.fail Timeout

      else (* we will wait for a bit *)
        Logs_lwt.app (fun m -> m "Sleeping for a time of %f, with an activity count of %f. %d tasks remaining" sleep_interval (!inactivity_count) (task_queue_size branch))
        >>= fun () -> Lwt_unix.sleep sleep_interval
        >|= (fun () -> (inactivity_count := !inactivity_count +. sleep_interval))
        >>= Lwt_main.yield
        >>= inner

    in inner ()

    (* Merge the map branch into master *)
    >>= fun () -> Store.merge_with_branch m
      ~info:(Irmin_unix.info ~author: "map" "Job %s complete" map_name) map_name
    >|= (fun merge -> match merge with
        | Ok () -> ()
        | Error _ -> invalid_arg "merge conflict")
    >>= fun () -> JobQueue.Impl.pop m
    >>= fun _ -> Logs_lwt.app (fun m -> m "Map operation complete. Branch name %s" map_name)

    >|= fun () -> m

  let () = Irmin_unix.set_listen_dir_hook ()
end
