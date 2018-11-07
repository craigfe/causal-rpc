open Lwt.Infix

module Store = Irmin_unix.Git.FS.KV(Irmin.Contents.String)
module Sync = Irmin.Sync(Store)

module type W = sig
  type value
  val run: ?name:string -> ?dir:string -> client:string -> unit -> unit Lwt.t
end

module Make (Eq : Map.EqualityType) (Op: Map.Operations with type t = Eq.t) = struct
  type value = Eq.t

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
    >>= fun master -> Store.set master
      ~info:(Irmin_unix.info ~author:"worker" "Initial commit")
      [".init"]
      "Some initial content"
    >>= fun () -> Sync.pull_exn master upstr `Set

    >|= fun _ -> while true do
      let lwt =
        (* Pull and check the map_request file for queued work *)
        Sync.pull_exn master upstr `Set
        >>= fun _ -> Store.find master ["map_request"]
        >>= fun branch -> match branch with

        (* A map request has been issued *)
        | Some br_name ->
          let _ = Logs.info (fun m -> m "Detected a map request on branch %s" br_name) in

          (* Checkout the branch *)
          let remote = upstream client br_name in
          Store.of_branch s br_name
          >>= fun local_br -> Sync.pull_exn local_br remote `Set
          >>= fun () -> Store.tree local_br

          (* TODO: Check that there is work to be done *)

          >>= (Logs.info (fun m -> m "Checked out branch %s" br_name);

               (* Get a list of all (key, value) pairs *)
               fun tree -> Store.Tree.list tree ["vals"])
          >>= Lwt_list.map_p (fun (key, _) -> Store.get local_br ["vals"; key]
                               >|= fun value -> Logs.warn (fun m -> m "k(%s) v(%s)" key value); (key, value))

          (* Perform the 'iter' operation to each value *)
          >>= Lwt_list.map_s (fun (key, value) -> (* HACK: parallelising this breaks for some reason *)
              Store.set local_br
                ~info:(Irmin_unix.info ~author:"client" "Performing work")
                ["vals"; key]
                (value |> Eq.of_string |> Op.iter |> Eq.to_string)
            )

          (* TODO: Set todo to false *)

          >|= (fun _ -> Logs.info (fun m -> m "Completed operation. Pushing changes to branch %s" br_name))

          (* Push the changes *)
          >>= fun _ -> Sync.push_exn local_br remote

          >|= (fun _ -> Logs.info (fun m -> m "Changes pushed to branch %s" br_name))

        | None ->
          Logs.debug (fun m -> m "Found no map request. Sleeping for %d seconds." poll_frequency);
          Unix.sleep poll_frequency;
          Lwt.return_unit

      in Lwt_main.run lwt
    done
end
