open Lwt.Infix

module Store = Irmin_unix.Git.FS.KV(Irmin.Contents.String)
module Sync = Irmin.Sync(Store)

(* let () =
 *   let lwt =
 *     Store.remote "https://github.com/mirage/irmin"
 *     |> fun upstr -> Store.Repo.v (Irmin_git.config ~bare:false "/tmp/irminmwe")
 *     >>= fun repo -> Store.master repo
 *     >>= fun master -> begin
 *       Printf.printf "Beginning pull #1\n";
 *       Sync.pull_exn master upstr `Set
 *     end
 *     >>= fun _ -> begin
 *       Printf.printf "Beginning pull #2\n";
 * 
 *       (\* Fatal error: exception (Invalid_argument "Sync.pull_exn: XXX") *\)
 *       Sync.pull_exn master upstr `Set
 *     end
 *   in Lwt_main.run lwt *)

(** Get an Irmin.store at a local or remote URI. *)
let upstream dir branch =
  let lwt =
    Irmin_git.config dir
    |> Store.Repo.v
    >>= fun repo -> Store.of_branch repo branch
    >|= Irmin.remote_store (module Store)
  in Lwt_main.run lwt

let init_remote () =
    Store.Repo.v (Irmin_git.config ~bare:true "/tmp/mwe1")
    >>= fun repo -> Store.master repo
    >>= fun master -> Store.set master
      ~info:(Irmin_unix.info ~author:"server" "signal")
      ["signal"] "foo"

    >>= fun _ -> Store.set master
      ~info:(Irmin_unix.info ~author:"server" "init value")
      ["vals"; "a"] "1"
    >>= fun _ -> Store.clone ~src:master ~dst:"foo"

    >>= fun foo -> Store.set foo
      ~info:(Irmin_unix.info ~author:"server" "msg")
      ["msg"] "Initial"

    >|= fun _ -> ()

let () =
  ignore (Sys.command ("rm -rf /tmp/mwe1 /tmp/mwe2"));
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Info);


  let lwt =
    let upstr = upstream "/tmp/mwe1" "master" in

    init_remote ()
    >>= fun _ -> Store.Repo.v (Irmin_git.config ~bare:true "/tmp/mwe2")
    >>= fun repo -> Store.master repo
    >>= fun master -> Sync.pull_exn master upstr `Set

    >>= fun () -> Store.get master ["signal"]
    >>= fun br_name -> let remote = upstream "/tmp/mwe1" br_name in
    Store.of_branch repo br_name

    >>= fun branch -> Sync.pull_exn branch remote `Set
    >>= fun () -> Store.get branch ["msg"]
    >>= fun msg -> Store.set branch
      ~info:(Irmin_unix.info ~author:"worker" "about to work") ["msg"] (msg ^ " about to work")
    >>= fun _ -> Sync.push_exn branch remote

    >>= fun _ -> Store.set branch
      ~info:(Irmin_unix.info ~author:"worker" "value set") ["vals"; "a"] "2"

    >>= fun _ -> Store.set branch
      ~info:(Irmin_unix.info ~author:"worker" "work done") ["msg"] ("Work done")

    >>= fun _ -> Sync.push_exn branch remote

    >>= fun _ -> Lwt.wrap (fun () -> Logs.app (fun m -> m "About to push"))
    >>= fun _ -> Sync.push_exn branch remote
    >|= fun _ -> ()

  in Lwt_main.run lwt


