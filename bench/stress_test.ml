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

  (* Create a simple client *)
  create_client (root ^ "clientA") (root ^ "server")
  >>= fun client -> IntMap.empty ~directory:(root ^ "server") ()
  >>= fun server -> IntMap.start server
  >>= (fun () ->
  let rec inner n max =
    if n = max then Lwt.return_unit
    else
      let init = Core.Time_ns.now () in
      IntClient.rpc increment_op Operation.Unit client
        >|= (fun _ -> let final = Core.Time_ns.now () in
              let span = Core.Time_ns.abs_diff init final in
              let (_, _, major_words) = Gc.counters () in

              print_string @@ Fmt.strf "%a,%a,%f\n" Core.Time_ns.pp final Core.Time_ns.Span.pp span major_words)

      >>= fun _ -> inner (n+1) max
  in inner 0 1000)

let () =
  (* Configure tracing *)
  let buffer = MProf_unix.mmap_buffer ~size:1000000 "trace.ctf" in
  let trace_config = MProf.Trace.Control.make buffer MProf_unix.timestamper in
  MProf.Trace.Control.start trace_config;

  (* Configure logging *)
  Trace_rpc.Helpers.set_reporter ();
  Logs.set_level (Some Logs.Info);

  Lwt_main.run (test_single_rpc ())

