open Lwt.Infix

module Store = Irmin_unix.Git.FS.KV(Irmin.Contents.String)
module Sync = Irmin.Sync(Store)

let server_config = Irmin_git.config ~bare:true "/tmp/irmin/server"
let config = Irmin_git.config ~bare:true "/tmp/irmin/worker"

(** Get an Irmin.store at a local or remote URI. *)
let upstream uri =
  if String.sub uri 0 7 = "file://" then
    let dir = String.sub uri 7 (String.length uri - 7) in
    let lwt =
      Irmin_git.config ~bare:true dir
      |> Store.Repo.v
      >>= Store.master
      >|= Irmin.remote_store (module Store)
    in Lwt_main.run lwt

  else
    Irmin.remote_uri uri

let test () =
  let upstr = upstream "file:///tmp/irmin/server" in

  Store.Repo.v config
  >>= Store.master
  >>= fun t  -> Sync.pull_exn t upstr `Set
  >>= fun () -> Store.get t ["test_file.md"]
  >|= fun r  -> Printf.printf "%s\n%!" r

let () = Lwt_main.run (test ())
