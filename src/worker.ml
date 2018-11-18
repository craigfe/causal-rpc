open Lwt.Infix

module type W = sig
  open Map

  include Map.S
  val get_task_opt: Sync.db -> Irmin.remote -> task option Lwt.t
  val perform_task: t -> task -> t
  val handle_request: Store.repo -> string -> JobQueue.job -> unit Lwt.t

  val run: ?name:string -> ?dir:string -> client:string -> unit -> unit Lwt.t
end

module Make (M : Map.S) (Impl: Interface.IMPL with type t = M.value) = struct
  include M

  module I = Interface.Implementation

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
      Irmin.remote_uri uri

  let random_name () =
    Misc.generate_rand_string ~length:8 ()
    |> Pervasives.(^) "worker_"
    |> fun x -> Logs.info (fun m -> m "No name supplied. Generating random worker name %s" x); x

  let directory_from_name name =
    let dir = "/tmp/irmin/" ^ name in
    Logs.info (fun m -> m "No directory supplied. Using default directory %s" dir);
    dir

  let get_task_opt local_br remote =
    Store.find local_br ["task_queue"]
    >>= fun q -> match q with
    | Some Task_queue ((x::xs), pending) ->
      Store.set local_br
        ~info:(Irmin_unix.info ~author:"map" "Consuming task")
        ["task_queue"]
        (Task_queue (xs, x::pending))
      >>= fun () -> Sync.push_exn local_br remote
      >|= fun () -> Some x

    | Some Task_queue ([], _) -> Lwt.return None (* All tasks are pending *)
    | None -> Lwt.return None (* No task to be performed *)
    | Some _ -> invalid_arg "Can't happen by design"

  (* TODO: do this as part of a transaction *)
  let remove_pending_task task m =
    Store.get m ["task_queue"]
    >|= (fun q -> match q with
        | Task_queue (todo, pending) -> Map.Task_queue (todo, List.filter (fun t -> t <> task) pending)
        | _ -> invalid_arg "Can't happen by design")
    >>= Store.set m ~info:(Irmin_unix.info ~author:"map" "Completed task") ["task_queue"]

  let perform_task map (key, operation) =
    let old_val = find key map in
    let operation = (match I.find_operation_opt operation Impl.api with
      | Some operation -> operation
      | None -> invalid_arg "Operation not found") in
    let new_val = operation old_val in
    add key new_val map

  let handle_request store client job =

    (* Checkout the branch *)
    let br_name = JobQueue.Impl.job_to_string job in
    let remote = upstream client br_name in
    Store.of_branch store br_name
    >>= fun local_br -> Sync.pull_exn local_br remote `Set
    >|= (fun () -> M.of_store local_br)

    (* Attempt to take a task from the queue *)
    >>= fun map -> get_task_opt local_br remote
    >>= (fun task -> match task with
        | None -> Lwt.return_unit (* No work to perform. We are done *)
        | Some t -> begin
            ignore (perform_task map t);
            remove_pending_task t local_br
          end)

    >|= (fun _ -> Logs.info (fun m -> m "Completed operation. Pushing changes to branch %s" br_name))

    (* Push the changes *)
    >>= (fun _ -> Sync.push_exn local_br remote)
    >|= (fun _ -> Logs.info (fun m -> m "Changes pushed to branch %s" br_name))

  let run
      ?(name=random_name())
      ?(dir=directory_from_name name)
      ~client () =

    Logs.app (fun m -> m "Initialising worker with name %s for client %s" name client);

    let config = Irmin_git.config ~bare:false dir in
    let upstr = upstream client "master" in
    let poll_frequency = 5 in

    Store.Repo.v config
    >>= fun s -> Store.master s
    >>= fun master -> Sync.pull_exn master upstr `Set

    >|= fun _ -> while true do
      let lwt =

        (* Pull and check the map_request file for queued jobs *)
        Sync.pull_exn master upstr `Set
        >>= fun _ -> JobQueue.Impl.peek_opt master
        >>= fun j -> match j with

        (* A map request has been issued *)
        | Some br_name ->

          Logs.app (fun m -> m "Detected a map request on branch %s" (JobQueue.Impl.job_to_string br_name));
          handle_request s client br_name

        | None ->
          Logs.debug (fun m -> m "Found no map request. Sleeping for %d seconds." poll_frequency);
          Unix.sleep poll_frequency;
          Lwt.return_unit

      in Lwt_main.run lwt
    done
end
