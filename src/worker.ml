open Lwt.Infix

module type W = sig
  val run:
    ?log_source:bool ->
    ?name:string ->
    ?dir:string ->
    ?poll_freq:float ->
    client:string -> unit -> unit Lwt.t
end

module Make (M : Map.S) (Impl: Interface.IMPL with module Val = M.Value): W = struct
  include M
  type value = Value.t

  module I = Interface.MakeImplementation(Impl.Val)

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

  let get_task_opt local_br working_br remote worker_name = (* TODO: implement this all in a transaction *)

    (* Get latest changes to this branch*)
    Sync.pull_exn local_br remote `Set
    >>= fun () -> Store.find local_br ["task_queue"]
    >>= fun q -> match q with
    | Some Task_queue ((x::xs), pending) ->
      Store.set working_br
        ~info:(Irmin_unix.info ~author:worker_name "Consume <%s> task on key %s" x.name x.key)
        ["task_queue"]
        (Task_queue (xs, x::pending))
      >>= fun res -> (match res with
          | Ok () -> Lwt.return_unit
          | Error se -> Lwt.fail @@ Store_error se)

      >|= fun () -> Some x

    | Some Task_queue ([], _) -> Lwt.return None (* All tasks are pending *)
    | None -> Lwt.return None (* No task to be performed *)
    | Some _ -> Lwt.fail Internal_type_error

  (* Take a store_tree and remove a pending task from it *)
  let remove_pending_task task (store_tree: Store.tree): Store.tree Lwt.t =

    Store.Tree.get store_tree ["task_queue"]
    >>= (fun q -> match q with
        | Task_queue tq -> Lwt.return tq
        | _ -> Lwt.fail Internal_type_error)
    >|= (fun (todo, pending) -> Map.Task_queue
            (todo, List.filter (fun t -> t <> task) pending))
    >>= Store.Tree.add store_tree ["task_queue"]

  (* We have a function of type (param -> ... -> param -> val -> val).
     Here we take the parameters that were passed as part of the RPC and recursively apply them
     to the function implementation until we are left with a function of type (val -> val). *)
  let pass_params ?src boxed_mi params =
    match boxed_mi with
    | I.Op.E matched_impl ->
      let (unboxed, func) = matched_impl in
      let func_type = I.Op.Unboxed.typ unboxed in

      (* We take a function type and a function _of that type_, and recursively apply parameters
         to the function until it reaches 'BaseType', i.e. val -> val *)
      let rec aux: type a.
        (value, a) Interface.func_type
        -> a
        -> Type.Boxed.t list
        -> (value -> value) = fun func_type func params ->

        match func_type with
        | Interface.BaseType -> (Logs.debug ?src (fun m -> m "Reached base type"); match params with
          | [] -> (fun x ->
              Logs.debug ?src (fun m -> m "Executing val -> val level function");
              let v = func x in
              Logs.debug ?src (fun m -> m "Function execution complete");
              v)
          | _ -> raise @@ Map.Malformed_params "Too many parameters")

        | Interface.ParamType (typ, nested_type) -> (Logs.debug ?src (fun m -> m "Nested type"); match params with
          | (x::xs) -> aux nested_type (func (Type.Boxed.unbox typ x)) xs
          | [] -> raise @@ Map.Malformed_params "Not enough parameters")

      in aux func_type func params

  (* Take a store tree and a task and return the tree with the operation performed *)
  let perform_task (task:Map.task) (store_tree: Store.tree): Store.tree Lwt.t =

    let boxed_mi () = (match I.find_operation_opt task.name Impl.api with
        | Some operation -> operation
        | None -> invalid_arg "Operation not found") in

    Store.Tree.find store_tree ["vals"; task.key]
    >>= (fun cont -> (match cont with
        | Some Value v -> Lwt.return v
        | Some _ -> Lwt.fail Internal_type_error
        | None -> Lwt.fail @@ Map.Protocol_error (Printf.sprintf "Value <%s> could not be found when attempting to perform %s operation" task.key task.name)))

    >>= fun old_val -> Lwt.return ((pass_params (boxed_mi ()) task.params) old_val)
    >>= fun new_val -> Store.Tree.add store_tree ["vals"; task.key] (Value new_val)

  let handle_request ?src repo client job worker_name =

    (* Checkout the branch *)
    let map_name = JobQueue.Impl.job_to_string job in
    let work_br_name = worker_name in
    let input_remote = upstream client map_name in
    let output_remote = upstream client work_br_name in

    Store.Branch.remove repo work_br_name (* We may have used this worker branch earlier. Delete it here to avoid problems *)
    >>= fun () -> Store.of_branch repo map_name

    (* We pull remote work into local_br, and perform the work on working_br. The remote then
       merges our work back into origin/local_br, completing the cycle. NOTE: The name of
       working_br must be unique, or pushed work overwrites others' and all hell breaks loose. *)
    >>= fun local_br -> Sync.pull_exn local_br input_remote `Set
    >>= fun () -> Store.clone ~src:local_br ~dst:(work_br_name)
    >>= fun working_br ->

    let rec task_exection_loop () =

      Sync.pull_exn local_br input_remote `Set
      >>= fun () -> Store.merge_with_branch working_br
        ~info:(Irmin_unix.info ~author:"worker_ERROR" "This should always be a fast-forward") map_name
      >>= fun res -> (match res with
          | Ok () -> Lwt.return_unit
          | Error `Conflict c ->
            Lwt.fail_with (Printf.sprintf "Conflict when attempting merge from %s into %s: %s" work_br_name map_name c))

      (* Attempt to take a task from the queue *)
      >>= fun () -> get_task_opt working_br working_br input_remote worker_name
      >>= fun task -> match task with

      | Some t -> begin
          Logs_lwt.info ?src (fun m -> m "Attempting to consume <%s> on key %s" t.name t.key)
          >>= fun () -> Sync.push working_br output_remote (* We first tell the remote that we intend to work on this item *)
          >>= fun res -> (match res with
              | Ok () -> Lwt.return_unit
              | Error pe -> Lwt.fail @@ Push_error pe
            )
          >>= fun () -> Logs_lwt.info ?src (fun m -> m "Starting to perform <%s> on key %s" t.name t.key)

          (* We get the trees with the performed task and the pending task removed *)
          >>= fun () -> Store.get_tree working_br []
          >>= perform_task t
          >>= remove_pending_task t
          >>= fun result_tree -> Logs_lwt.info ?src @@ fun m -> m "Completed <%s> task on key %s. Pushing to remote" t.name t.key

          (* Now perform the commit and push to the remote. This commit should never be empty. But for
             debugging purposes we show empty commits *)
          >>= fun () -> Store.set_tree ~allow_empty:true
            ~info:(Irmin_unix.info ~author:worker_name "Perform <%s> task on key %s" t.name t.key)
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

      | None -> Logs_lwt.info ?src @@ fun m -> m "No available tasks in the task queue."

    in task_exection_loop ()

  let run
      ?(log_source=true)
      ?(name=random_name())
      ?dir
      ?(poll_freq = 5.0)
      ~client () =

    if String.sub name 0 5 |> String.equal "map--" then
      invalid_arg "Worker names cannot begin with map--";

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

    Logs_lwt.app ?src (fun m -> m "Initialising worker with name %s for client %s" name client)
    >>= fun () -> Store.Repo.v config
    >>= fun s -> Store.master s

    >>= fun master ->

    let rec inner () =

      (* Pull and check the map_request file for queued jobs *)
      Sync.pull_exn master upstr `Set
      >>= fun () -> JobQueue.Impl.peek_opt master
      >>= fun j -> (match j with

          (* A map request has been issued *)
          | Some br_name ->

            Logs_lwt.info ?src (fun m -> m "Detected a map request on branch %s"
                              (JobQueue.Impl.job_to_string br_name))
            >>= fun () -> handle_request ?src s client br_name name
            >>= fun () -> Logs_lwt.info ?src (fun m -> m "Finished handling request on branch %s"
                                            (JobQueue.Impl.job_to_string br_name))

          | None ->
            Logs_lwt.info ?src (fun m -> m "Found no map request. Sleeping for %f seconds." poll_freq)
            >>= fun () -> Lwt_unix.sleep poll_freq)

      >>= Lwt_main.yield
      >>= inner

    in inner ()
end
