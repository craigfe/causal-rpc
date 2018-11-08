open Lwt.Infix

module type W = sig
  type value
  val run: ?name:string -> ?dir:string -> client:string -> unit -> unit Lwt.t
end

module Make (Val : Irmin.Contents.S) (Impl: Interface.IMPL with type t = Val.t) = struct
  module Map = Map.Make(Val)(Op)

  module Contents = Map.Contents
  module Store = Map.Store
  module Sync = Map.Sync

  type value = Val.t

  (** Get an Irmin.store at a local or remote URI. *)
  let upstream uri branch =
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

  (* Checkout br_name in store and pull from client, then perform any work still to
     do on that branch *)
  let handle_request store client br_name =

    (* Checkout the branch *)
    let remote = upstream client br_name in
    Store.of_branch store br_name
    >>= fun local_br -> Sync.pull_exn local_br remote `Set
    >|= (fun () -> Map.of_store local_br)

    (* TODO: Check that there is work to be done *)

    (* Get a list of all (key, value) pairs *)
    >|= (fun map -> (map, List.map (fun k -> (k, Map.find k map)) (Map.keys map)))

    (* Perform the 'iter' operation to each value *)
    >|= (fun (map, kvs) -> List.iter
            (fun (key, value) -> ignore (Map.add key (Op.iter value) map)) kvs)

    (* TODO: Set todo to false *)

    >|= (fun _ -> Logs.info (fun m -> m "Completed operation. Pushing changes to branch %s" br_name))

    (* Push the changes *)
    >>= fun _ -> Sync.push_exn local_br remote
    >|= fun _ -> Logs.info (fun m -> m "Changes pushed to branch %s" br_name)

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

        (* Pull and check the map_request file for queued work *)
        Sync.pull_exn master upstr `Set
        >>= fun _ -> Store.find master ["map_request"]
        >>= fun branch -> match branch with

        (* A map request has been issued *)
        | Some br_name_val ->

          (match br_name_val with
           | Branch_name br_name ->
             Logs.info (fun m -> m "Detected a map request on branch %s" br_name);
             handle_request s client br_name
           | _ -> invalid_arg "Can't happen by design")

        | None ->
          Logs.debug (fun m -> m "Found no map request. Sleeping for %d seconds." poll_frequency);
          Unix.sleep poll_frequency;
          Lwt.return_unit

      in Lwt_main.run lwt
    done
end
