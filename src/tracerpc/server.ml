open Lwt.Infix

module Store = Irmin_unix.Git.FS.KV(Irmin.Contents.String)
module Sync = Irmin.Sync(Store)

(* Commit information *)
let info author fmt = Irmin_unix.info ~author:author fmt

let random_server_name () =
  Misc.generate_rand_string ()
  |> Pervasives.(^) "server--"
	|> fun x -> Logs.info (fun m -> m "No server name supplied. Generated random name %s" x); x

let print_commit c =
  print_endline (Irmin.Info.message (Store.Commit.info c))

let print_commit_type commit_diff = match commit_diff with
  | `Updated (a, b) -> print_endline " --- updated --- "; print_commit a; print_commit b
  | `Removed c -> print_endline " --- removed --- "; print_commit c
  | `Added c -> print_endline " --- added --- "; print_commit c

let callback commit =
  Logs.warn (fun m -> m "Callback invoked");
  print_commit_type commit;
  let _ = Printf.sprintf "Change detected at time %s" (Misc.timestamp ()) in
  Lwt.return_unit

let commit t name n =
  Unix.sleep 1;
  Logs.info (fun m -> m "Performing commit #%d" n);
  Store.set t ~info:(info name "cmt %d" n) [string_of_int n] (Printf.sprintf "%s: %n" (Misc.timestamp ()) n)

let run ?(name=random_server_name ()) port =
  Logs.info (fun m -> m "Started with name %s and port %d at time %s" name port (Misc.timestamp ()));

  let poll_rate = 1.0 in
  let directory = "/tmp/irmin/test" in
  let config = Irmin_git.config ~bare:true directory in

  Irmin_watcher.set_polling_time poll_rate;
  Irmin_unix.set_listen_dir_hook ();

  Store.Repo.v config
  >>= Store.master
  >>= fun t -> Store.set t ~info:(info name "Initial commit") [".init"] (Printf.sprintf "[%s] : port %d" (Misc.timestamp ()) port)
  >>= fun res -> (match res with
  | Ok () -> Store.watch t callback
  | Error _ -> invalid_arg "some error")

  (* Commit a few things to the repository *)
  >>= fun w -> commit t name 1
  (* >>= fun () -> commit t name 2
   * >>= fun () -> commit t name 3
   * >>= fun () -> commit t name 4
   * >>= fun () -> commit t name 5 *)

  >>= fun _ -> Unix.sleep 10000; Store.unwatch w

let main name port =
  Lwt_main.run (run ?name port)


