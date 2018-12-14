open Lwt.Infix

exception Empty_queue

type ('v, 'jq) contents =
  | Value of 'v
  | Task_queue of Task_queue.t
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

  let t =
    let open Irmin.Type in
    variant "contents" (fun value task_queue branch_name -> function
        | Value v -> value v
        | Task_queue q -> task_queue q
        | Job_queue js -> branch_name js)
    |~ case1 "Value" Val.t (fun v -> Value v)
    |~ case1 "Task_queue" Task_queue.t (fun q -> Task_queue q)
    |~ case1 "Job_queue" JQueueType.t (fun js -> Job_queue js)
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

exception Malformed_params of string
exception Protocol_error of string
exception Timeout

module type S = sig

  module Value: Irmin.Contents.S

  type key = string
  type queue

  type t

  module Contents: Irmin.Contents.S with type t = (Value.t, queue) contents
  module Store: Store.S
    with type key = Irmin.Path.String_list.t
     and type step = string
     and module Key = Irmin.Path.String_list
     and type contents = Contents.t
     and type branch = string

  module Sync: Irmin.SYNC with type db = Store.t
  module JobQueue: JOB_QUEUE with module Store = Store
  module Operation: Interface.OPERATION with module Val = Value

  exception Internal_type_error
  exception Store_error of Store.write_error
  type 'a params = 'a Interface.MakeOperation(Value).params

  (* Here for testing purposes *)
  val generate_task_queue: 'a Operation.Unboxed.t -> 'a params -> t -> (Value.t, queue) contents Lwt.t
  (* ------------------------- *)

  val of_store: Sync.db -> t
  val to_store: t -> Sync.db

  val empty: ?directory:string -> ?remote_uri:string -> unit -> t Lwt.t
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
    (GitBackend: Irmin_git.G)
    (Desc: Interface.DESC)
    (QueueType: QUEUE_TYPE)
    (JQueueMake: functor
       (Val: Irmin.Contents.S)
       (St: Store.S
        with type key = Irmin.Path.String_list.t
         and type step = string
         and module Key = Irmin.Path.String_list
         and type contents = (Val.t, QueueType.t) contents
         and type branch = string)
       -> (JOB_QUEUE with module Store = St)
    ): S
  with module Value = Desc.Val
   and module Operation = Interface.MakeOperation(Desc.Val)
   and type queue = QueueType.t = struct


  module Value = Desc.Val
  module Contents = MakeContents(Desc.Val)(QueueType)
  module Store = Store.Make(GitBackend)(Contents)
  module Sync = Irmin.Sync(Store)
  module JobQueue = JQueueMake(Desc.Val)(Store)
  module Operation = Interface.MakeOperation(Desc.Val)

  type key = string
  type value = Value.t
  type queue = QueueType.t
  type 'a params = 'a Operation.params

  type t = {
    local: Store.t;
    remote_uri: string option;
  }
  (* A map is a branch in an Irmin Key-value store *)

  exception Internal_type_error
  exception Store_error of Store.write_error

  let generate_random_directory () =
    Misc.generate_rand_string ~length:20 ()
    |> Pervasives.(^) "/tmp/irmin/set/"
    |> fun x -> Logs.info (fun m -> m "No directory supplied. Generated random directory %s" x); x

  let empty ?(directory=generate_random_directory()) ?remote_uri () =
    let config = Irmin_git.config ~bare:true directory in

    if String.sub directory 0 11 <> "/tmp/irmin/"
    then invalid_arg ("Supplied directory (" ^ directory ^ ") must be in /tmp/irmin/");

    (* Delete the directory if it already exists... Unsafe! *)
    let ret_code = Sys.command ("rm -rf " ^ directory) in
    if (ret_code <> 0) then invalid_arg "Unable to delete directory";

    Store.Repo.v config
    >>= Store.master

    >|= fun local -> {local; remote_uri}

  let of_store s = {local = s; remote_uri = None}
  let to_store {local; _} = local

  let mem key {local; _} =
    Store.tree local
    >>= fun tree -> Store.Tree.list tree ["vals"]
    >|= List.exists (fun (x,_) -> x = key)

  let add ?message key value m =
    let l = m.local in
    let message = (match message with
        | Some m -> m
        | None -> Printf.sprintf "Commit to key %s" key) in

    Store.set
      ~allow_empty:true
      ~info:(Irmin_unix.info ~author:"client" "%s" message)
      l
      ["vals"; key]
      (Value value)

    >>= fun res -> match res with
    | Ok () -> Lwt.return m
    | Error se -> Lwt.fail @@ Store_error se

  let add_all ?message kv_list m =
    let l = m.local in
    let rec contains_duplicates l = (match l with
        | [] -> false
        | x::xs -> (List.mem x xs) || contains_duplicates xs)

    in if contains_duplicates kv_list then invalid_arg "Duplicate keys in key/value list";

    let message = (match message with
        | Some m -> m
        | None -> Printf.sprintf "Commit to %d keys" (List.length kv_list)) in


    Store.find_tree l ["vals"]
    >|= (fun t -> match t with
        | Some t -> t
        | None -> Store.Tree.empty)

    (* We construct the commit by folding over the (k,v) list and accumulating a tree *)
    >>= Lwt_list.fold_right_s
      (fun (k, v) tree_acc -> Store.Tree.add tree_acc [k] (Value v))
      kv_list

    >>= fun tree -> Store.set_tree
      ~allow_empty:true
      ~info:(Irmin_unix.info ~author:"client" "%s" message)
      l ["vals"] tree

    >>= fun res -> (match res with
        | Ok () -> Lwt.return m
        | Error se -> Lwt.fail @@ Store_error se)

  let find key {local; _} =
    (* Get the value from the store and deserialise it *)
    Store.find local ["vals"; key]

    >>= fun value -> match value with
    | Some (Value v) -> Lwt.return v
    | Some _ -> Lwt.fail Internal_type_error
    | None -> Lwt.fail Not_found

  let remove _ _ = invalid_arg "TODO"

  let size {local; _} =
    Store.tree local
    >>= fun tree -> Store.Tree.list tree ["vals"]
    (* >|= List.filter (fun (_, typ) -> typ = `Contents) *)
    >|= List.length

  let is_empty m =
    size m
    >|= (=) 0

  let keys m =
    Store.tree m.local
    >>= fun tree -> Store.Tree.list tree ["vals"]
    >|= List.map(fst)

  let values m =
    Store.tree m.local
    >>= fun tree -> Store.Tree.list tree ["vals"]
    >>= Lwt_list.map_p (fun (x, _) -> Store.get m.local ["vals"; x])
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

  let rec flatten_params: type a. a params -> Type.Boxed.t list = fun ps ->
    match ps with
    | Interface.Unit -> []
    | Interface.Param (typ, p, ps) -> ((Type.Boxed.box typ p)::flatten_params(ps))

  let generate_task_queue: type a. a Operation.Unboxed.t -> a params -> t -> (value, queue) contents Lwt.t = fun operation params map ->
    let open Task_queue in
    let name = Operation.Unboxed.name operation in
    let param_list = flatten_params params in

    keys map
    >|= List.map (fun key -> {name; params = param_list; key})
    >>= fun ops -> Logs_lwt.app (fun m -> m "Generated task queue of [%s]"
                                    (List.map show_task ops |> String.concat ", "))
    >|= fun () -> Task_queue (ops, []) (* Initially there are no pending operations *)

  let set_task_queue q m =
    Store.set ~info:(Irmin_unix.info ~author:"map" "Specify workload")
      m ["task_queue"] q

    >>= fun res -> match res with
    | Ok () -> Lwt.return_unit
    | Error we -> Lwt.fail @@ Store_error we

  let map: type a. ?timeout:float -> a Operation.Unboxed.t -> a params -> t -> t Lwt.t =
    fun ?(timeout=5.0) operation params m ->

    let l = m.local in

    (* Generate a new unique branch name for the map *)
    let rec unique_name_gen () =
      Lwt.wrap (fun () -> "map--" ^ Misc.generate_rand_string ~length:8 ())
      >>= fun map_name -> Store.Branch.mem (Store.repo l) map_name
      >>= fun exists -> if exists then unique_name_gen () else Lwt.return map_name
    in unique_name_gen ()

    >>= fun map_name -> Logs_lwt.app (fun m -> m "Map operation issued. Branch name %s" map_name)

    (* Push the job to the job queue *)
    >>= fun () -> JobQueue.Impl.push (JobQueue.Impl.job_of_string map_name) l

    (* Create a new branch to isolate the operation *)
    >>= fun () -> Store.clone ~src:l ~dst:map_name
    >>= fun branch -> Store.merge_with_branch l
      ~info:(Irmin_unix.info ~author:"map" "Merged") Store.Branch.master
    >>= Misc.handle_merge_conflict Store.Branch.master map_name

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
    let watch_callback (br_name: Store.branch) (_: Sync.commit Irmin.diff) =

      (* Here we assume that all branches that don't start with 'map--' are worker branches
         for this map request. In future this may not always be the case *)
      if String.equal br_name map_name then Lwt.return_unit
      else if not (String.sub br_name  0 5 |> String.equal "map--") then begin

        Store.of_branch (Store.repo l) br_name
        >>= JobQueue.Impl.peek_opt
        >>= fun job -> (match job with
            | Some j when String.equal map_name (JobQueue.Impl.job_to_string j) ->

              Logs_lwt.debug (fun m -> m "Resetting count due to activity on branch %s" br_name)
              >|= (fun () -> inactivity_count := 0.0)

              (* Merge the work from this branch *)
              >>= fun () -> Store.merge_with_branch branch
                ~info:(Irmin_unix.info ~author:"map" "Merged work from %s into %s" br_name map_name) br_name

              >>= Misc.handle_merge_conflict br_name map_name

            | Some j -> Logs_lwt.warn
                          (fun m -> m "Woke up due to submitted work for a job %s, but the currently executing job is %s"
                              (JobQueue.Impl.job_to_string j) map_name)

            | None -> Lwt.fail @@ Protocol_error (Printf.sprintf "Received work on branch %s, but there is no job on this branch" br_name))

      end else
        Logs_lwt.warn (fun m -> m "Woke up due to an irrelevant branch %s when waiting for work on %s" br_name map_name)

    in

    Store.Branch.watch_all (Store.repo l) watch_callback

    >>= fun watch -> Logs_lwt.app (fun m -> m "Waiting for the task queue to be empty, with timeout %f" timeout)
    >>= fun () ->

    let sleep_interval = ref 0.01 in
    let rec inner () =

      task_queue_is_empty branch
      >>= fun is_empty -> if is_empty then (* we are done *)
        Store.unwatch watch

      else if !inactivity_count >= timeout then (* we have been waiting for too long *)
        Logs_lwt.app (fun m -> m "Inactivity count: %f" (!inactivity_count))
        >>= fun () -> Lwt.fail Timeout

      else (* we will wait for a bit *)
        task_queue_size branch
        >>= fun tq_size -> Logs_lwt.app (fun m -> m "Sleeping for a time of %f, with an inactivity count of %f. %d tasks remaining"
                                            !sleep_interval !inactivity_count tq_size)
        >>= fun () -> Lwt_unix.sleep (!sleep_interval)
        >|= (fun () ->
            inactivity_count := !inactivity_count +. !sleep_interval;
            sleep_interval := !sleep_interval *. 2.)
        >>= inner

    in inner ()

    >>= (fun () -> Logs_lwt.info @@ fun m -> m "All operations complete on %s. Merging with the master branch" map_name)

    (* Merge the map branch into master *)
    >>= fun () -> Store.merge_with_branch l
      ~info:(Irmin_unix.info ~author: "map" "Job %s complete" map_name) map_name
    >>= Misc.handle_merge_conflict map_name Store.Branch.master

    (* Remove the job from the job queue *)
    >>= fun () -> JobQueue.Impl.pop l

    (* For now, we only ever perform one map at once. Eventually, the job queue
       will need to be cleverer to avoid popping off the wrong job here *)
    >>= fun j -> if JobQueue.Impl.job_equal j (JobQueue.Impl.job_of_string map_name)
    then Lwt.fail_with "Didn't pop the right job!"
    else Lwt.return_unit

      >>= (fun _ -> Logs_lwt.app @@ fun m -> m "Map operation complete. Branch name %s" map_name)
      >|= fun () -> m

  let () = Irmin_unix.set_listen_dir_hook ()
end
