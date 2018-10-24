open Lwt.Infix

module Store = Irmin_unix.Git.FS.KV(Irmin.Contents.String)
module Sync = Irmin.Sync(Store)

(* Commit information *)
let info author fmt = Irmin_unix.info ~author:author fmt

let generate_random_name ?(length=8) () =
	Random.self_init (); (* Initialise the random number generator *)

  let rand_char_code () = match Random.int(26+26+10) with
      n when n < 26      -> int_of_char 'a' + n
    | n when n < 26 + 26 -> int_of_char 'A' + n - 26
    | n                  -> int_of_char '0' + n - 26 - 26 in

  let rand_string _ = rand_char_code ()
                      |> char_of_int
                      |> (String.make 1) in

  Array.init length rand_string
	|> Array.to_list
	|> List.cons "server--"
	|> String.concat ""
	|> fun x -> Logs.info (fun m -> m "No server name supplied. Generated random name %s" x); x

let timestamp () =
  let ts = Unix.gettimeofday() in
  let tm = Unix.localtime ts in
  let us, _s = modf ts in
  Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d.%03d "
    (1900 + tm.Unix.tm_year)
    (1    + tm.Unix.tm_mon)
    (tm.Unix.tm_mday)
    (tm.Unix.tm_hour)
    (tm.Unix.tm_min)
    (tm.Unix.tm_sec)
    (int_of_float (1_000. *. us))

(* let print_commit c =
 *   print_endline (Irmin.Info.message (Store.Commit.info c)) *)

let print_commit_type commit_diff = match commit_diff with
  | `Updated (_, _) -> print_endline "updated"
  | `Removed _ -> print_endline "removed"
  | `Added _ -> print_endline "added"

let callback commit =
  Logs.warn (fun m -> m "Callback invoked");
  print_commit_type commit;
  let _ = Printf.sprintf "Change detected at time %s" (timestamp ()) in
  Lwt.return_unit

let commit t name n =
  Unix.sleep 1;
  Logs.info (fun m -> m "Performing commit #%d" n);
  Store.set t ~info:(info name "cmt %d" n) [string_of_int n] (Printf.sprintf "%s: %n" (timestamp()) n)

let run ?(name=generate_random_name ()) port =
  Logs.info (fun m -> m "Started with name %s and port %d at time %s" name port (timestamp ()));

  let poll_rate = 1.0 in
  let directory = "/tmp/irmin/test" in
  let config = Irmin_git.config ~bare:true directory in

  Irmin_watcher.set_polling_time poll_rate;
  Irmin_unix.set_listen_dir_hook ();

  Store.Repo.v config
  >>= Store.master
  >>= fun t -> Store.set t ~info:(info name "Initial commit") [".init"] (Printf.sprintf "[%s] : port %d" (timestamp ()) port)
  >>= fun () -> Store.watch t callback

  (* Commit a few things to the repository *)
  >>= fun w -> commit t name 1
  >>= fun () -> commit t name 2
  >>= fun () -> commit t name 3
  >>= fun () -> commit t name 4
  >>= fun () -> commit t name 5
  >>= fun () -> Unix.sleep 100; Store.unwatch w

let main name port =
  Lwt_main.run (run ?name port)


