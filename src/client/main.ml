open Lwt.Infix

module Store = Irmin_unix.Git.FS.KV(Irmin.Contents.String)
module Sync = Irmin.Sync(Store)
let config = Irmin_git.config ~bare:true "/tmp/irmin/test"

let upstream =
  if Array.length Sys.argv = 2 then (Irmin.remote_uri Sys.argv.(1))
  else (Printf.eprintf "Usage: sync [uri]\n%!"; exit 1)

let test () =
  Store.Repo.v config >>= Store.master
  >>= fun t  -> Sync.pull_exn t upstream `Set
  >>= fun () -> Store.get t ["README.md"]
  >|= fun r  -> Printf.printf "%s\n%!" r

let () = Lwt_main.run (test ())
