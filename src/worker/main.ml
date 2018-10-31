open Lwt.Infix

module Store = Irmin_unix.Git.FS.KV(Irmin.Contents.String)
module Sync = Irmin.Sync(Store)
let config = Irmin_git.config ~bare:true "/tmp/irmin/worker"

let upstream =
  (* This works *)
  (* Irmin.remote_uri "git@github.com:CraigFe/trace-rpc.git" *)

  (* This does not *)
  Irmin.remote_uri "file:///tmp/irmin/server"

let test () =
  Store.Repo.v config
  >>= Store.master
  >>= fun t  -> Sync.pull_exn t upstream `Set
  >>= fun () -> Store.get t ["test_file.md"]
  >|= fun r  -> Printf.printf "%s\n%!" r

let () = Lwt_main.run (test ())
