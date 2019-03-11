open Lwt.Infix
open Task_queue
open Contents
open Task

let random_name ?src () =
  Misc.generate_rand_string ~length:8 ()
  |> Pervasives.(^) "worker--"
  |> fun x -> Logs.info ?src (fun m -> m "No name supplied. Generating random worker name %s" x); x

module Config = struct
  type t = {
    log_source: bool;
    random_selection: bool;
    batch_size: int;
    thread_count: int;
    name: string;
    poll_freq: float;
    two_phase: bool;
  }

  let def opt d = match opt with
    | Some v -> v
    | None -> d

  let make ?log_source ?random_selection ?batch_size
      ?thread_count ?name ?poll_freq ?two_phase () =
    {
      log_source       = def log_source true;
      random_selection = def random_selection true;
      batch_size       = def batch_size 1;
      thread_count     = def thread_count 1;
      name             = def name (random_name());
      poll_freq        = def poll_freq 5.0;
      two_phase        = def two_phase true
    }

  let log_source t       = t.log_source
  let random_selection t = t.random_selection
  let batch_size t       = t.batch_size
  let thread_count t     = t.thread_count
  let name t             = t.name
  let poll_freq t        = t.poll_freq
  let two_phase t        = t.two_phase
end

module type W = sig
  val run:
    ?switch:Lwt_switch.t ->
    ?config:Config.t ->
    ?dir:string ->
    client:string -> unit -> unit Lwt.t
end

module Make
    (M: Map.S)
    (Impl: Interface.IMPL with module Val = M.Value): W = struct

  include M
  module C = Config
  module I = Interface.MakeImplementation(Impl.Val)
  module E = Executor.Make(I)


  let directory_from_name ?src name =
    let dir = "/tmp/irmin/" ^ name in
    Logs.info ?src (fun m -> m "No directory supplied. Using default directory %s" dir);
    dir

  let get_tasks_opt ~random_selection ~batch_size working_br remote worker_name = (* TODO: implement this all in a transaction *)

    Store.IrminStore.find working_br ["task_queue"]
    >>= fun q -> match q with
    | Some Task_queue (_::_ as todo, pending) ->

      let rec can_take_all_tasks l n = match l, n with
        | [], _ -> true
        | _, 0 -> false
        | _::ts, n -> can_take_all_tasks ts (n-1) in

      let split_func =
        if can_take_all_tasks todo batch_size then (* Consume all the tasks if we can *)
          (fun _ a -> (a, []))

        else if random_selection then (* If random_selection is enabled, use that *)
          Misc.split_random

        else (* Otherwise pick sequentially from the list *)
          Misc.split_sequential in

      let (selected, remaining) = split_func batch_size todo in
      let pp_task = Fmt.of_to_string (fun (t:Task.t) -> Fmt.strf "<%s> on key %s" t.name t.key) in
      let pp_commit = Fmt.of_to_string (fun (t:Task.t list) -> match t with
          | _::_::_ -> Fmt.strf "Consume [%a]" (Fmt.list ~sep:Fmt.comma pp_task) t
          | _       -> Fmt.strf "Consume %a"   (Fmt.list ~sep:Fmt.comma pp_task) t) in

      Store.IrminStore.set working_br ~info:(Store.B.make_info ~author:worker_name "%a" pp_commit selected)
        ["task_queue"] (Task_queue (remaining, selected @ pending))

      >>= fun res -> (match res with
          | Ok () -> Lwt.return_unit
          | Error se -> Lwt.fail @@ Store.Store_error se)

      >|= fun () -> selected

    | Some Task_queue ([], _) -> Lwt.return [] (* All tasks are pending *)
    | None -> Lwt.return [] (* No task to be performed *)
    | Some _ -> Lwt.fail Exceptions.Internal_type_error

  let perform_task ?src (store_tree: Store.IrminStore.tree) (task:Task.t): (Task.t * Value.t) Lwt.t =
    let boxed_mi () = (match I.find_operation_opt task.name Impl.api with
        | Some operation -> operation
        | None -> invalid_arg "Operation not found") in

    Store.IrminStore.Tree.find store_tree ["vals"; task.key]
    >>= (fun cont -> (match cont with
        | Some Value v -> Lwt.return v
        | Some _ -> Lwt.fail Exceptions.Internal_type_error
        | None -> Lwt.fail @@ Exceptions.Protocol_error (Printf.sprintf "Value <%s> could not be found when attempting to perform %s operation" task.key task.name)))

    >>= fun old_val -> E.execute_task ?src (boxed_mi ()) task.params old_val
    >|= fun new_val -> (task, new_val)


  (* Take a store tree and list of tasks and return the tree with the tasks performed *)
  let perform_tasks ?src (tasks:Task.t list) (store_tree: Store.IrminStore.tree): Store.IrminStore.tree Lwt.t =
    let add_task_result ((t, v): Task.t * Value.t) tree =
      Store.IrminStore.Tree.add tree ["vals"; t.key] (Value v) in

    Lwt_list.map_p (perform_task ?src store_tree) tasks
    >>= fun tasks -> Lwt_list.fold_right_s add_task_result tasks store_tree

  let handle_pull_errors r = match r with
       | Ok () -> Lwt.return_unit
       | Error `Conflict msg -> Logs_lwt.err (fun m -> m "Conflict <%s> when attempting to pull remote work into local_br" msg)
       | Error `Msg msg -> Logs_lwt.err (fun m -> m "Error message <%s> when attempting to pull remote work into local_br" msg)
       | Error `No_head -> Logs_lwt.err (fun m -> m "No head when attempting to pull remote work into local_br")
       | Error `Not_available -> Logs_lwt.err (fun m -> m "Not_available when attempting to pull remote work into local_br")

  let handle_request
      ~random_selection
      ~batch_size
      ~two_phase
      ?src repo client map_name worker_name =

    (* Checkout the branch *)
    let work_br_name = worker_name in

    let pp_task = Fmt.of_to_string (fun (t:Task.t) -> Fmt.strf "<%s> on key %s" t.name t.key) in
    let pp_commit = Fmt.of_to_string (fun (t:Task.t list) -> match t with
        | _::_::_ -> Fmt.strf "Perform [%a]" (Fmt.list ~sep:Fmt.comma pp_task) t
        | _       -> Fmt.strf "Perform %a"   (Fmt.list ~sep:Fmt.comma pp_task) t) in

    Store.upstream client map_name
    >>= fun input_remote -> Store.upstream client work_br_name
    >>= fun output_remote ->

    Store.IrminStore.Branch.remove repo work_br_name (* We may have used this worker branch earlier. Delete it here to avoid problems *)
    >>= fun () -> Store.IrminStore.of_branch repo map_name

    (* We pull remote work into local_br, and perform the work on working_br. The remote then
       merges our work back into origin/local_br, completing the cycle. NOTE: The name of
       working_br must be unique, or pushed work overwrites others' and all hell breaks loose. *)
    >>= fun local_br -> Sync.pull local_br input_remote `Set
    >>= handle_pull_errors

    >>= fun () -> Store.IrminStore.clone ~src:local_br ~dst:work_br_name
    >>= fun working_br ->

    let rec task_exection_loop () =
      let info = Store.B.make_info ~author:"worker_ERROR" "This should always be a fast-forward" in

      Sync.pull local_br input_remote (`Merge info)
      >>= handle_pull_errors

      >>= fun () -> Store.IrminStore.merge_with_branch working_br
        ~info:(Store.B.make_info ~author:worker_name "Updating world-view on %s" map_name)
        map_name
      >>= Misc.handle_merge_conflict work_br_name map_name

      (* Attempt to take a task from the queue *)
      >>= fun () -> get_tasks_opt ~random_selection ~batch_size working_br input_remote worker_name
      >>= fun tasks -> match tasks with

      | [] -> Logs_lwt.info ?src @@ fun m -> m "No available tasks in the task queue."
      | ts -> begin
          let task_count = List.length ts in
          let fmt_tasklist = Fmt.brackets @@ Fmt.list ~sep:Fmt.comma Task.pp in

          assert (task_count <= batch_size);

          (match task_count with
           | 1             -> Logs_lwt.info ?src (fun m -> m "Attempting to consume: %a" Task.pp (List.hd ts))
           | n when n < 11 -> Logs_lwt.info ?src (fun m -> m "Attempting to consume %d tasks: %a" n fmt_tasklist ts)
           | n             -> Logs_lwt.info ?src (fun m -> m "Attempting to consume %d tasks" n))

          >>= (fun () -> if two_phase then
                  Sync.push working_br output_remote (* We first tell the remote that we intend to work on this item *)
                  >>= fun res -> (match res with
                      | Ok () -> Lwt.return_unit
                      | Error pe -> Lwt.fail @@ Store.Push_error pe
                    )
                else Lwt.return_unit)

          >>= fun () -> Logs_lwt.info ?src (fun m -> m "Starting to perform %a" fmt_tasklist ts)

          (* We get the trees with the tasks having been a) performed to the map and b) removed from the pending queue *)
          >>= fun () -> Store.IrminStore.get_tree working_br []
          >>= perform_tasks ts
          >>= Store.remove_pending_tasks ts

          (* XXX: If we use Logs_lwt here, and set the verbosity higher than info, the worker fails to push the results *)
          >|= (fun result_tree -> (Logs.info ?src @@ fun m -> m "Completed %a. Pushing to remote" fmt_tasklist ts); result_tree)

          (* Now perform the commit and push to the remote. This commit should never be empty. But for
             debugging purposes we show empty commits *)
          >>= fun result_tree -> Store.IrminStore.set_tree ~allow_empty:true
            ~info:(Store.B.make_info ~author:worker_name "%a" pp_commit ts)
            working_br [] result_tree

          >>= fun res -> (match res with
              | Ok () -> Lwt.return_unit
              | Error se -> Lwt.fail @@ Store.Store_error se)

          >>= fun () -> Sync.push working_br output_remote
          >>= fun res -> (match res with
              | Ok () -> Lwt.return_unit
              | Error pe -> Lwt.fail @@ Store.Push_error pe)

          >>= fun () -> Logs_lwt.info ?src @@ fun m -> m "Changes pushed to branch %s" map_name
          >>= Store.B.yield
          >>= task_exection_loop
        end
    in task_exection_loop ()


  let run ?switch ?config:(conf = C.make ()) ?dir ~client () =
    let name = C.name conf in
    let random_selection = C.random_selection conf in
    let batch_size = C.batch_size conf in
    let thread_count = C.thread_count conf in
    let poll_freq = C.poll_freq conf in
    let two_phase = C.two_phase conf in

    if String.sub name 0 5 |> String.equal "map--" then
      invalid_arg "Worker names cannot begin with map--";

    if batch_size < 1 then invalid_arg "The batch size must be positive";
    if poll_freq <= 0.0 then invalid_arg "The polling frequency must be positive";

    if random_selection then Random.self_init (); (* Initialise the random number generator *)

    let src = if C.log_source conf then Some (Logs.Src.create name) else None in
    let dir = match dir with
      | Some d -> d
      | None -> directory_from_name ?src name in
    let config = Irmin_git.config ~bare:false dir in

    Logs_lwt.debug (fun m -> m "Beginning to initialise worker")

    >>= fun () -> Logs.debug (fun m -> m "Checking that %s has a safe prefix" dir);

    if String.sub dir 0 11 <> "/tmp/irmin/" then
       Lwt.fail_with ("Supplied directory (" ^ dir ^ ") must be in /tmp/irmin/")
     else Lwt.return_unit

    (* Delete the directory if it already exists... Unsafe! *)
    >>= fun () -> Logs_lwt.debug (fun m -> m "Deleting pre-existing directory")
    >>= fun () -> let ret_code = Sys.command ("rm -rf " ^ dir) in
    (if ret_code <> 0 then
       Lwt.fail_with "Unable to delete directory"
     else Lwt.return_unit)

    (* Initialise the task executor *)
    >>= fun () -> Logs_lwt.debug (fun m -> m "Initialising the task executor")
    >>= fun () -> Lwt.return (E.initialise ?src ~thread_count)
    >>= fun () ->

    (* Store a set of the jobs we have completed before *)
    (* TODO: prevent this from growing infinitely? *)
    let module SS = Set.Make(String) in
    let seen_before = ref SS.empty in

    Logs_lwt.app ?src (fun m -> m "Initialising worker with name %s for client %s" name client)
    >>= fun () -> Logs_lwt.app ?src (fun m -> m "Worker repo contained in %s" dir)
    >>= fun () -> Store.IrminStore.Repo.v config
    >>= fun s -> Store.IrminStore.master s
    >>= fun master -> Logs_lwt.debug ?src (fun m -> m "Store constructed")

    >>= fun () -> Store.upstream client "master"
    >>= fun upstr ->

    let rec inner () =

      (* First check that the switch is on, if it exists *)
      (match switch with
       | Some s -> if Lwt_switch.is_on s then Lwt.return_unit else Lwt.fail_with "Switch is off"
       | None -> Lwt.return_unit)

      (* Pull and check the map_request file for queued jobs *)
      >>= fun () -> Sync.pull master upstr (`Merge (Store.B.make_info ~author:"worker_ERROR" "This should always be a fast-forward"))
      >>= handle_pull_errors

      >>= fun () -> Store.JobQueue.Impl.peek_opt master
      >>= fun j -> Logs_lwt.debug (fun m -> m "Branches seen before: %a"
                                      (Fmt.brackets @@ Fmt.list ~sep:Fmt.comma Fmt.string) (SS.elements !seen_before))
      >>= fun () -> (match j with

          (* A map request has been issued *)
          | Some (Job.MapJob br_name) ->
            if SS.mem br_name !seen_before then
              Logs_lwt.debug ?src (fun m -> m "Detected map request <%s>, but we have already seen this" br_name)
            else
              Logs_lwt.info ?src (fun m -> m "Detected a new map request on branch %s" br_name)
              >>= fun () -> handle_request ~random_selection ~batch_size ~two_phase ?src s client br_name name
              >|= (fun () -> seen_before := SS.add br_name !seen_before)
              >>= fun () -> Logs_lwt.info ?src (fun m -> m "Finished handling request on branch %s" br_name)

          | Some (Job.Rpc _) | None ->
            Logs_lwt.info ?src (fun m -> m "Found no map request. Sleeping for %f seconds." poll_freq)
            >>= fun () -> Store.B.sleep poll_freq)

      >>= Store.B.yield
      >>= inner

    in inner ()
end
