open Lwt.Infix

exception Empty_queue

type task = {
  name: string;
  params: Type.Boxed.t list;
  key: string;
} [@@deriving show]

let task =
  let open Irmin.Type in
  record "task" (fun name params key -> { name; params; key })
  |+ field "name" string (fun t -> t.name)
  |+ field "params" (list Type.Boxed.irmin_t) (fun t -> t.params)
  |+ field "key" string (fun t -> t.key)
  |> sealr

type task_queue = (task list * task list) [@@deriving show]
type ('v, 'jq) contents =
  | Value of 'v
  | Task_queue of task_queue
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
    val job_equal: job -> job -> bool

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

  let task_queue_t = Irmin.Type.(pair (list task) (list task))

  let t =
    let open Irmin.Type in
    variant "contents" (fun value task_queue branch_name -> function
        | Value v -> value v
        | Task_queue q -> task_queue q
        | Job_queue js -> branch_name js)
    |~ case1 "Value" Val.t (fun v -> Value v)
    |~ case1 "Task_queue" task_queue_t (fun q -> Task_queue q)
    |~ case1 "Job_queue" JQueueType.t (fun js -> Job_queue js)
    |> sealv

  let merge_values = Irmin.Merge.(idempotent Val.t)
  let merge_task_queues = Irmin.Merge.(idempotent task_queue_t)

  let merge ~old t1 t2 =

    let open Irmin.Merge.Infix in
    old () >>=* fun old ->

    match (old, t1, t2) with
    | Some Value o, Value a, Value b ->

      (Irmin.Merge.f merge_values) ~old:(Irmin.Merge.promise o) a b
      >>=* fun x -> Irmin.Merge.ok (Value x)

    | Some Task_queue o, Task_queue a, Task_queue b ->

      (Irmin.Merge.f merge_task_queues) ~old:(Irmin.Merge.promise o) a b
      >>=* fun x -> Irmin.Merge.ok (Task_queue x)

    (* Irmin.Merge.conflict "%s" (Format.asprintf "old = %a\n\na = %a\n\nb = %a" pp_task_queue o pp_task_queue a pp_task_queue b) *)

    | _, Job_queue _, Job_queue _ -> Irmin.Merge.conflict "Job_queue"

    | _ -> Irmin.Merge.conflict "Different values"

  let merge = Irmin.Merge.(option (v t merge))

end

exception Malformed_params of string
exception Protocol_error of string
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

  exception Internal_type_error
  exception Store_error of Store.write_error
  type 'a params = 'a Interface.MakeOperation(Value).params

  (* Here for testing purposes *)
  val task_queue_is_empty: t -> bool Lwt.t
  val job_queue_is_empty: t -> bool Lwt.t
  val generate_task_queue: 'a Operation.Unboxed.t -> 'a params -> t -> (Value.t, queue) contents Lwt.t
  (* ------------------------- *)

  val of_store: Sync.db -> t
  val to_store: t -> Sync.db

  val empty: ?directory:string -> unit -> t Lwt.t
  val is_empty: t -> bool Lwt.t
  val mem: key -> t -> bool Lwt.t
  val add: ?message:string -> key -> Value.t -> t -> t Lwt.t
  val add_all: ?message:string -> (key * Value.t) list -> t -> t Lwt.t
  val find: key -> t -> Value.t Lwt.t
  val remove: key -> t -> t Lwt.t
  val size: t -> int Lwt.t
  val keys: t -> key list Lwt.t
  val values: t -> Value.t list Lwt.t
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

  exception Internal_type_error
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

      Store.Repo.v config
      >>= fun repo -> Store.of_branch repo "master"
    end

  let of_store s = s
  let to_store s = s

  let mem key m =
    Store.tree m
    >>= fun tree -> Store.Tree.list tree ["vals"]
    >|= List.exists (fun (x,_) -> x = key)

  let add ?message key value m =
    let message = (match message with
        | Some m -> m
        | None -> Printf.sprintf "Commit to key %s" key) in

    Store.set
      ~allow_empty:true
      ~info:(Irmin_unix.info ~author:"client" "%s" message)
      m
      ["vals"; key]
      (Value value)

    >>= fun res -> match res with
    | Ok () -> Lwt.return m
    | Error se -> Lwt.fail @@ Store_error se

  let add_all ?message kv_list m =
    let rec contains_duplicates l = (match l with
      | [] -> false
      | x::xs -> (List.mem x xs) || contains_duplicates xs)

    in if contains_duplicates kv_list then invalid_arg "Duplicate keys in key/value list";

    let message = (match message with
        | Some m -> m
        | None -> Printf.sprintf "Commit to %d keys" (List.length kv_list)) in

    (* We construct the commit by folding over the (k,v) list and accumulating a tree *)
    Lwt_list.fold_right_s
      (fun (k, v) tree_acc -> Store.Tree.add tree_acc ["vals"; k] (Value v))
      kv_list
      Store.Tree.empty

    >>= fun tree -> Store.set_tree
      ~allow_empty:true
      ~info:(Irmin_unix.info ~author:"client" "%s" message)
      m [] tree

    >>= fun res -> (match res with
    | Ok () -> Lwt.return m
    | Error se -> Lwt.fail @@ Store_error se)

  let find key m =
      (* Get the value from the store and deserialise it *)
      Store.find m ["vals"; key]

      >>= fun value -> match value with
      | Some (Value v) -> Lwt.return v
      | Some _ -> Lwt.fail Internal_type_error
      | None -> Lwt.fail Not_found

  let remove _ _ = invalid_arg "TODO"

  let size m =
    Store.tree m
    >>= fun tree -> Store.Tree.list tree ["vals"]
    (* >|= List.filter (fun (_, typ) -> typ = `Contents) *)
    >|= List.length

  let is_empty m =
    size m
    >|= (=) 0

  let keys m =
      Store.tree m
      >>= fun tree -> Store.Tree.list tree ["vals"]
      >|= List.map(fst)

  let values m =
    Store.tree m
    >>= fun tree -> Store.Tree.list tree ["vals"]
    >>= Lwt_list.map_p (fun (x, _) -> Store.get m ["vals"; x])
    >|= List.map (fun value -> match value with
        | Value v -> v
        | _ -> raise Internal_type_error
      )

  let get_task_queue m =
    Store.find m ["task_queue"]
    >>= fun q -> match q with
    | Some Task_queue tq -> Lwt.return tq
    | _ -> Lwt.fail Internal_type_error

  let task_queue_is_empty branch =
    get_task_queue branch
    >|= fun q -> match q with
    | ([], []) -> true
    | _ -> false

  let task_queue_size branch =
    get_task_queue branch
    >|= fun (a, b) -> (List.length a) + (List.length b)

  let job_queue_is_empty m =
    JobQueue.Impl.is_empty m

  let rec flatten_params: type a. a params -> Type.Boxed.t list = fun ps ->
    match ps with
    | Interface.Unit -> []
    | Interface.Param (typ, p, ps) -> ((Type.Boxed.box typ p)::flatten_params(ps))

  let generate_task_queue: type a. a Operation.Unboxed.t -> a params -> t -> (value, queue) contents Lwt.t = fun operation params map ->
    let name = Operation.Unboxed.name operation in
    let param_list = flatten_params params in

    keys map
    >|= List.map (fun key -> {name; params = param_list; key})
    >|= (fun ops ->
        Logs.app (fun m -> m "Generated task queue of [%s]"
                     (List.map (fun {name = n; params = _; key = k} ->
                          Printf.sprintf "{name: %s; key: %s}" n k) ops
                      |> String.concat ", "));
        ops)
    >|= fun ops -> Task_queue (ops, []) (* Initially there are no pending operations *)

  let set_task_queue q m =
    Store.set ~info:(Irmin_unix.info ~author:"map" "Specify workload")
      m ["task_queue"] q

    >>= fun res -> match res with
    | Ok () -> Lwt.return_unit
    | Error we -> Lwt.fail @@ Store_error we

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
      ~info:(Irmin_unix.info ~author:"map" "Merged") Store.Branch.master
    >>= (fun merge -> match merge with
        | Ok () -> Lwt.return_unit
        | Error `Conflict key -> Lwt.fail_with ("merge conflict on key " ^ key))

    (* Generate and commit the task queue *)
    >>= fun () -> generate_task_queue operation params m
    >>= fun tq -> set_task_queue tq branch

    (* Wait for the task queue to be empty *)
    >>= fun () ->

    let inactivity_count = ref 0.0 in

    (* The callback to be executed when we detect changes to the repository.
       Note: here we don't explicitly signal the workers that are available for a
       map, so we have to watch for any changes in the repo and then filter within
       the callback. *)
    let watch_callback (br: Store.branch) (_: Sync.commit Irmin.diff) =

      (* First, we check that this wakeup is to do with this branch *)
      String.length map_name
      |> String.sub br 0
      |> String.equal map_name
      |> fun relevant_branch -> if relevant_branch then begin

        Logs.warn (fun m -> m "Resetting count due to activity on branch %s" br);
        inactivity_count := 0.0;

        (* Merge the work from this branch *)
        Store.merge_with_branch branch
          ~info:(Irmin_unix.info ~author:"map" "Merged work from %s into %s" br map_name) br

        >>= fun res -> (match res with
            | Ok () -> Lwt.return_unit
            | Error `Conflict key -> Lwt.fail_with ("merge conflict on key " ^ key))

      end else
        Logs_lwt.warn (fun m -> m "Woke up due to an irrelevant branch %s when waiting for work on %s" br map_name)

    in

    Store.Branch.watch_all (Store.repo m) watch_callback

    >>= fun watch -> Logs_lwt.app (fun m -> m "Waiting for the task queue to be empty, with timeout %f" timeout)
    >>= fun () ->
    let rec inner () =

      let sleep_interval = Pervasives.min (timeout /. 8.0) 1.0 in

      task_queue_is_empty branch
      >>= fun is_empty -> if is_empty then (* we are done *)
        Store.unwatch watch

      else if !inactivity_count >= timeout then (* we have been waiting for too long *)
        Logs_lwt.app (fun m -> m "Inactivity count: %f" (!inactivity_count))
        >>= fun () -> Lwt.fail Timeout

      else (* we will wait for a bit *)
        task_queue_size branch
        >>= fun tq_size -> Logs_lwt.app (fun m -> m "Sleeping for a time of %f, with an inactivity count of %f. %d tasks remaining"
                                            sleep_interval (!inactivity_count) tq_size)
        >>= fun () -> Lwt_unix.sleep sleep_interval
        >|= (fun () -> (inactivity_count := !inactivity_count +. sleep_interval))
        >>= Lwt_main.yield
        >>= inner

    in inner ()

    >>= (fun () -> Logs_lwt.info @@ fun m -> m "All operations complete on %s. Merging with the master branch" map_name)

    (* Merge the map branch into master *)
    >>= fun () -> Store.merge_with_branch m
      ~info:(Irmin_unix.info ~author: "map" "Job %s complete" map_name) map_name
    >>= (fun merge -> match merge with
        | Ok () -> Lwt.return_unit
        | Error `Conflict key -> Lwt.fail_with ("merge conflict on key " ^ key))

    (* Remove the job from the job queue *)
    >>= fun () -> JobQueue.Impl.pop m

    (* For now, we only ever perform one map at once. Eventually, the job queue
       will need to be cleverer to avoid popping off the wrong job here *)
    >>= fun j -> if JobQueue.Impl.job_equal j (JobQueue.Impl.job_of_string map_name)
    then Lwt.fail_with "Didn't pop the right job!"
    else Lwt.return_unit

    >>= (fun _ -> Logs_lwt.app @@ fun m -> m "Map operation complete. Branch name %s" map_name)
    >|= fun () -> m

  let () = Irmin_unix.set_listen_dir_hook ()
end
