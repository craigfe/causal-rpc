open Lwt.Infix

module type W = sig
  open Map

  include Map.S
  val get_task_opt: Sync.db -> Irmin.remote -> string -> task option Lwt.t
  val perform_task: t -> task -> t
  val handle_request: Store.repo -> string -> JobQueue.job -> string -> unit Lwt.t

  val run:
    ?name:string ->
    ?dir:string ->
    ?poll_freq:float ->
    client:string -> unit -> unit Lwt.t
end

module Make (M : Map.S) (Impl: Interface.IMPL with module Val = M.Value): W = struct
  include M
  type value = Value.t

  module I = Interface.MakeImplementation(Impl.Val)

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

  let random_name () =
    Misc.generate_rand_string ~length:8 ()
    |> Pervasives.(^) "worker_"
    |> fun x -> Logs.info (fun m -> m "No name supplied. Generating random worker name %s" x); x

  let directory_from_name name =
    let dir = "/tmp/irmin/" ^ name in
    Logs.info (fun m -> m "No directory supplied. Using default directory %s" dir);
    dir

  let get_task_opt local_br remote worker_name =
    Store.find local_br ["task_queue"]
    >>= fun q -> match q with
    | Some Task_queue ((x::xs), pending) ->
      Store.set local_br
        ~info:(Irmin_unix.info ~author:worker_name "Consuming task on key %s" x.key)
        ["task_queue"]
        (Task_queue (xs, x::pending))
      >>= fun res -> (match res with
      | Ok () -> Sync.push_exn local_br remote
      | Error _ -> invalid_arg "some error")

      >|= fun () -> Some x

    | Some Task_queue ([], _) -> Lwt.return None (* All tasks are pending *)
    | None -> Lwt.return None (* No task to be performed *)
    | Some _ -> invalid_arg "Can't happen by design"

  (* TODO: do this as part of a transaction *)
  let remove_pending_task task local_br worker_name =
    Store.get local_br ["task_queue"]
    >|= (fun q -> match q with
        | Task_queue (todo, pending) -> Map.Task_queue (todo, List.filter (fun t -> t <> task) pending)
        | _ -> invalid_arg "Can't happen by design")

    >>= Store.set local_br
      ~info:(Irmin_unix.info ~author:worker_name "Removed pending <%s> on key %s" task.name task.key)
      ["task_queue"]
    >|= fun res -> (match res with
    | Ok () -> ()
    | Error _ -> invalid_arg "some error")

  (* We have a function of type (param -> ... -> param -> val -> val).
     Here we take the parameters that were passed as part of the RPC and recursively apply them
     to the function implementation until we are left with a function of type (val -> val). *)
  let pass_params boxed_mi params =
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
          | Interface.BaseType -> (match params with
              | [] -> func
              | _ -> invalid_arg "Too many parameters")

          | Interface.ParamType (typ, nested_type) -> (match params with
            | (x::xs) -> aux nested_type (func (Type.Boxed.unbox typ x)) xs
            | [] -> invalid_arg "Not enough parameters")

      in aux func_type func params

  let perform_task map (task:Map.task) =
    Logs.info (fun m -> m "Performing the task");

    let old_val = find task.key map in
    let boxed_mi = (match I.find_operation_opt task.name Impl.api with
      | Some operation -> operation
      | None -> invalid_arg "Operation not found") in

    let new_val = (pass_params boxed_mi task.params) old_val in
    add ~message:("Performed task on key " ^ task.key) task.key new_val map

  let handle_request repo client job worker_name =

    (* Checkout the branch *)
    let br_name = JobQueue.Impl.job_to_string job in
    let remote = upstream client br_name in
    let simp_log msg = Lwt.wrap (fun () -> Logs.info (fun m -> m msg)) in

    Store.of_branch repo br_name
    >>= fun local_br -> Sync.pull_exn local_br remote `Set

    (* Attempt to take a task from the queue *)
    >>= fun () -> get_task_opt local_br remote worker_name
    >>= (fun task -> match task with
        | Some t -> begin
            Lwt.wrap (fun () -> perform_task (of_store local_br) t)
            >>= fun _ -> simp_log "Completed task. Removing from pending queue"
            >>= fun () -> remove_pending_task t local_br worker_name
            >>= fun () -> simp_log "Removed task from pending queue"
            >>= fun _ -> Sync.push_exn local_br remote
            >|= fun () -> Logs.info (fun m -> m "Changes pushed to branch %s" br_name)
          end

        | None -> begin
            Logs.info (fun m -> m "No pending tasks in the task queue.");
            Lwt.return_unit
          end)

  let run
      ?(name=random_name())
      ?(dir=directory_from_name name)
      ?(poll_freq = 5.0)
      ~client () =

    Logs.app (fun m -> m "Initialising worker with name %s for client %s" name client);

    let config = Irmin_git.config ~bare:false dir in
    let upstr = upstream client "master" in

    Store.Repo.v config
    >>= fun s -> Store.master s

    >|= fun master -> while true do
      let lwt =

        (* Pull and check the map_request file for queued jobs *)
        Sync.pull_exn master upstr `Set
        >>= fun _ -> JobQueue.Impl.peek_opt master
        >>= fun j -> match j with

        (* A map request has been issued *)
        | Some br_name ->

          Lwt.wrap (fun () -> Logs.info (fun m -> m "Detected a map request on branch %s"
                                            (JobQueue.Impl.job_to_string br_name)))

          >>= fun () -> handle_request s client br_name name
          >|= fun () -> Logs.info (fun m -> m "Finished handling request on branch %s"
                                      (JobQueue.Impl.job_to_string br_name))


        | None ->
          Logs.info (fun m -> m "Found no map request. Sleeping for %d seconds." poll_frequency);
          Unix.sleep poll_frequency;
          Lwt.return_unit

      in Lwt_main.run lwt
    done
end
