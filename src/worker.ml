open Lwt.Infix

module type W = sig
  val run:
    ?switch:Lwt_switch.t ->
    ?log_source:bool ->
    ?random_selection:bool ->
    ?batch_size:int ->
    ?thread_count:int ->
    ?name:string ->
    ?dir:string ->
    ?poll_freq:float ->
    client:string -> unit -> unit Lwt.t
end

module Make (M : Map.S) (Impl: Interface.IMPL with module Val = M.Value): W = struct
  include M

  module I = Interface.MakeImplementation(Impl.Val)
  module E = Executor.Make(I)

  exception Push_error of Sync.push_error

  (** Get an Irmin.store at a local or remote URI. *)
  let upstream uri branch =

    (* It's currently necessary to manually case-split between 'local' remotes
       using the file local protocol and the standard git protocol
       https://github.com/mirage/irmin/issues/589 *)

    if String.sub uri 0 7 = "file://" then
      let dir = String.sub uri 7 (String.length uri - 7) in
      let lwt =
        Irmin_git.config dir
        |> Store.Repo.v
        >>= fun repo -> Store.of_branch repo branch
        >|= Irmin.remote_store (module Store)
      in Lwt_main.run lwt
    else
      Store.remote uri


  let random_name ?src () =
    Misc.generate_rand_string ~length:8 ()
    |> Pervasives.(^) "worker--"
    |> fun x -> Logs.info ?src (fun m -> m "No name supplied. Generating random worker name %s" x); x


  let directory_from_name ?src name =
    let dir = "/tmp/irmin/" ^ name in
    Logs.info ?src (fun m -> m "No directory supplied. Using default directory %s" dir);
    dir


  let get_tasks_opt ~random_selection ~batch_size working_br remote worker_name = (* TODO: implement this all in a transaction *)

    Store.find working_br ["task_queue"]
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
      let pp_task = Fmt.of_to_string (fun (t:Task_queue.task) -> Fmt.strf "<%s> on key %s" t.name t.key) in
      let pp_commit = Fmt.of_to_string (fun (t:Task_queue.task list) -> match t with
          | _::_::_ -> Fmt.strf "Consume [%a]" (Fmt.list ~sep:Fmt.comma pp_task) t
          | _       -> Fmt.strf "Consume %a"   (Fmt.list ~sep:Fmt.comma pp_task) t) in

      Store.set working_br ~info:(Irmin_unix.info ~author:worker_name "%a" pp_commit selected)
        ["task_queue"] (Task_queue (remaining, selected @ pending))

      >>= fun res -> (match res with
          | Ok () -> Lwt.return_unit
          | Error se -> Lwt.fail @@ Store_error se)

      >|= fun () -> selected

    | Some Task_queue ([], _) -> Lwt.return [] (* All tasks are pending *)
    | None -> Lwt.return [] (* No task to be performed *)
    | Some _ -> Lwt.fail Internal_type_error


  (* Take a store_tree and remove a pending task from it *)
  let remove_pending_task (task: Task_queue.task) (store_tree: Store.tree): Store.tree Lwt.t =

    Store.Tree.get store_tree ["task_queue"]
    >>= (fun q -> match q with
        | Task_queue tq -> Lwt.return tq
        | _ -> Lwt.fail Internal_type_error)
    >|= (fun (todo, pending) -> Map.Task_queue
            (todo, List.filter (fun t -> t <> task) pending))
    >>= Store.Tree.add store_tree ["task_queue"]


  let remove_pending_tasks (tasks: Task_queue.task list) (store_tree: Store.tree): Store.tree Lwt.t =
    Lwt_list.fold_right_s remove_pending_task tasks store_tree


  let perform_task ?src (store_tree: Store.tree) (task:Task_queue.task): (Task_queue.task * Value.t) Lwt.t =
    let boxed_mi () = (match I.find_operation_opt task.name Impl.api with
        | Some operation -> operation
        | None -> invalid_arg "Operation not found") in

    Store.Tree.find store_tree ["vals"; task.key]
    >>= (fun cont -> (match cont with
        | Some Value v -> Lwt.return v
        | Some _ -> Lwt.fail Internal_type_error
        | None -> Lwt.fail @@ Map.Protocol_error (Printf.sprintf "Value <%s> could not be found when attempting to perform %s operation" task.key task.name)))

    >>= fun old_val -> E.execute_task ?src (boxed_mi ()) task.params old_val
    >|= fun new_val -> (task, new_val)


  (* Take a store tree and list of tasks and return the tree with the tasks performed *)
  let perform_tasks ?src (tasks:Task_queue.task list) (store_tree: Store.tree): Store.tree Lwt.t =
    let add_task_result ((t, v): Task_queue.task * Value.t) tree =
      Store.Tree.add tree ["vals"; t.key] (Value v) in

    Lwt_list.map_p (perform_task ?src store_tree) tasks
    >>= fun tasks -> Lwt_list.fold_right_s add_task_result tasks store_tree


  let handle_request ~random_selection ~batch_size ?src repo client job worker_name =

    (* Checkout the branch *)
    let map_name = JobQueue.Impl.job_to_string job in
    let work_br_name = worker_name in
    let input_remote = upstream client map_name in
    let output_remote = upstream client work_br_name in

    let pp_task = Fmt.of_to_string (fun (t:Task_queue.task) -> Fmt.strf "<%s> on key %s" t.name t.key) in
    let pp_commit = Fmt.of_to_string (fun (t:Task_queue.task list) -> match t with
        | _::_::_ -> Fmt.strf "Perform [%a]" (Fmt.list ~sep:Fmt.comma pp_task) t
        | _       -> Fmt.strf "Perform %a"   (Fmt.list ~sep:Fmt.comma pp_task) t) in

    Store.Branch.remove repo work_br_name (* We may have used this worker branch earlier. Delete it here to avoid problems *)
    >>= fun () -> Store.of_branch repo map_name

    (* We pull remote work into local_br, and perform the work on working_br. The remote then
       merges our work back into origin/local_br, completing the cycle. NOTE: The name of
       working_br must be unique, or pushed work overwrites others' and all hell breaks loose. *)
    >>= fun local_br -> Sync.pull_exn local_br input_remote `Set
    >>= fun () -> Store.clone ~src:local_br ~dst:work_br_name
    >>= fun working_br ->

    let rec task_exection_loop () =
      let info = Irmin_unix.info ~author:"worker_ERROR" "This should always be a fast-forward" in

      Sync.pull_exn local_br input_remote (`Merge info)
      >>= fun () -> Store.merge_with_branch working_br
        ~info:(Irmin_unix.info ~author:worker_name "Updating world-view on %s" map_name)
        map_name
      >>= Misc.handle_merge_conflict work_br_name map_name

      (* Attempt to take a task from the queue *)
      >>= fun () -> get_tasks_opt ~random_selection ~batch_size working_br input_remote worker_name
      >>= fun tasks -> match tasks with

      | [] -> Logs_lwt.info ?src @@ fun m -> m "No available tasks in the task queue."
      | ts -> begin
          let task_count = List.length ts in
          let fmt_tasklist = Fmt.brackets @@ Fmt.list ~sep:Fmt.comma Task_queue.pp_task in

          assert (task_count <= batch_size);

          (match task_count with
           | 1             -> Logs_lwt.info ?src (fun m -> m "Attempting to consume: %a" Task_queue.pp_task (List.hd ts))
           | n when n < 11 -> Logs_lwt.info ?src (fun m -> m "Attempting to consume %d tasks: %a" n fmt_tasklist ts)
           | n             -> Logs_lwt.info ?src (fun m -> m "Attempting to consume %d tasks" n))

          >>= fun () -> Sync.push working_br output_remote (* We first tell the remote that we intend to work on this item *)
          >>= fun res -> (match res with
              | Ok () -> Lwt.return_unit
              | Error pe -> Lwt.fail @@ Push_error pe
            )
          >>= fun () -> Logs_lwt.info ?src (fun m -> m "Starting to perform %a" fmt_tasklist ts)

          (* We get the trees with the tasks having been a) performed to the map and b) removed from the pending queue *)
          >>= fun () -> Store.get_tree working_br []
          >>= perform_tasks ts
          >>= remove_pending_tasks ts

          (* XXX: If we use Logs_lwt here, and set the verbosity higher than info, the worker fails to push the results *)
          >|= (fun result_tree -> (Logs.info ?src @@ fun m -> m "Completed %a. Pushing to remote" fmt_tasklist ts); result_tree)

          (* Now perform the commit and push to the remote. This commit should never be empty. But for
             debugging purposes we show empty commits *)
          >>= fun result_tree -> Store.set_tree ~allow_empty:true
            ~info:(Irmin_unix.info ~author:worker_name "%a" pp_commit ts)
            working_br [] result_tree

          >>= fun res -> (match res with
              | Ok () -> Lwt.return_unit
              | Error se -> Lwt.fail @@ Store_error se)

          >>= fun () -> Sync.push working_br output_remote
          >>= fun res -> (match res with
              | Ok () -> Lwt.return_unit
              | Error pe -> Lwt.fail @@ Push_error pe)

          >>= fun () -> Logs_lwt.info ?src @@ fun m -> m "Changes pushed to branch %s" map_name
          >>= Lwt_main.yield
          >>= task_exection_loop
        end


    in task_exection_loop ()

  let run
      ?switch
      ?(log_source=true)
      ?(random_selection=false)
      ?(batch_size=1)
      ?(thread_count=1)
      ?(name=random_name())
      ?dir
      ?(poll_freq = 5.0)
      ~client () =

    if String.sub name 0 5 |> String.equal "map--" then
      invalid_arg "Worker names cannot begin with map--";

    if batch_size < 1 then invalid_arg "The batch size must be positive";
    if poll_freq <= 0.0 then invalid_arg "The polling frequency must be positive";

    if random_selection then Random.self_init (); (* Initialise the random number generator *)

    let src = if log_source then Some (Logs.Src.create name) else None in
    let dir = match dir with
      | Some d -> d
      | None -> directory_from_name ?src name in

    let config = Irmin_git.config ~bare:false dir in
    let upstr = upstream client "master" in

    if String.sub dir 0 11 <> "/tmp/irmin/"
    then invalid_arg ("Supplied directory (" ^ dir ^ ") must be in /tmp/irmin/");

    (* Delete the directory if it already exists... Unsafe! *)
    let ret_code = Sys.command ("rm -rf " ^ dir) in
    if (ret_code <> 0) then invalid_arg "Unable to delete directory";

    (* Initialise the task executor *)
    E.initialise ?src ~thread_count;

    Logs_lwt.app ?src (fun m -> m "Initialising worker with name %s for client %s" name client)
    >>= fun () -> Store.Repo.v config
    >>= fun s -> Store.master s

    >>= fun master ->

    let rec inner () =

      (* First check that the switch is on, if it exists *)
      (match switch with
       | Some s -> if Lwt_switch.is_on s then Lwt.return_unit else Lwt.fail_with "Switch is off"
       | None -> Lwt.return_unit)

      (* Pull and check the map_request file for queued jobs *)
      >>= fun () -> Sync.pull_exn master upstr (`Merge (Irmin_unix.info ~author:"worker_ERROR" "This should always be a fast-forward"))
      >>= fun () -> JobQueue.Impl.peek_opt master
      >>= fun j -> (match j with

          (* A map request has been issued *)
          | Some br_name ->

            Logs_lwt.info ?src (fun m -> m "Detected a map request on branch %s"
                                   (JobQueue.Impl.job_to_string br_name))
            >>= fun () -> handle_request ~random_selection ~batch_size ?src s client br_name name
            >>= fun () -> Logs_lwt.info ?src (fun m -> m "Finished handling request on branch %s"
                                                 (JobQueue.Impl.job_to_string br_name))

          | None ->
            Logs_lwt.info ?src (fun m -> m "Found no map request. Sleeping for %f seconds." poll_freq)
            >>= fun () -> Lwt_unix.sleep poll_freq)

      >>= Lwt_main.yield
      >>= inner

    in inner ()
end
