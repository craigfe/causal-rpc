open Lwt.Infix
open Trace_rpc
open Intmap
open Core

module GitBackend = Irmin_unix.Git.Mem.G
module I = IntPair (Trace_rpc_unix.Make)(GitBackend)
open I

let create_client directory remote =
  IntClient.empty ~directory
    ~local_uri:("file://" ^ directory)
    ~remote_uri:("file://" ^ remote)
    ~name:"clientA"
    ~initial:Int64.one

let test_single_rpc () =
  Random.self_init ();
  (* Trace_rpc.Helpers.set_reporter (); *)
  (* Logs.set_level (Some Logs.Info); *)
  let root = "/tmp/irmin/test_unicast/single_rpc/" in

  (* let () = Irmin.Watch.set_listen_dir_hook Irmin.Watch.none in
   * let () = Irmin_watcher.set_polling_time 0.01 in *)

  (* Create a simple client *)
  create_client (root ^ "clientA") (root ^ "server")
  >>= fun client -> IntMap.empty ~directory:(root ^ "server") ()
  >>= fun server -> IntMap.start server
  >>= (fun () ->
  let rec inner n max =
    if n = max then Lwt.return_unit
    else
      let init = Core.Time_ns.now () in
      IntClient.rpc ~timeout:1000. (O.apply increment_op) client
        >|= (fun _ -> let final = Core.Time_ns.now () in
              let span = Core.Time_ns.abs_diff init final in

              let (_, _, major_words) = Gc.counters () in
              print_string @@ Fmt.strf "%a,%a,%f\n" Core.Time_ns.pp final Core.Time_ns.Span.pp span major_words)

        (* print_string @@ Fmt.strf "%a\n" Core.Time_ns.Span.pp span) *)

        >>= fun _ -> if n mod 50_000 = 0
        then IntClient.clear_caches client
        else Lwt.return_unit

        >>= fun () -> inner (n+1) max
  in inner 0 1_000_000)

  (* >|= fun () -> print_endline (Fmt.strf "%d" (Irmin_watcher.stats ()).dispatches) *)

let () =
  (* let () = Gc.tune ~max_overhead:1000000 () in *)

  (* Configure tracing *)
  (* let buffer = MProf_unix.mmap_buffer ~size:1000000 "trace.ctf" in
   * let trace_config = MProf.Trace.Control.make buffer MProf_unix.timestamper in
   * MProf.Trace.Control.start trace_config; *)

  (* Configure logging *)
  (* Trace_rpc.Helpers.set_reporter ();
   * Logs.set_level (Some Logs.Info); *)

  Lwt_main.run (test_single_rpc ())

