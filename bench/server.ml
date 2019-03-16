open Lwt.Infix
open Trace_rpc

module GitBackend = Irmin_unix.Git.FS.G
module I = Intmap.IntPair (Trace_rpc_unix.Make)(GitBackend)
open I

let () =
  (* Configure tracing *)
  let buffer = MProf_unix.mmap_buffer ~size:1000000 "server.ctf" in
  let trace_config = MProf.Trace.Control.make buffer MProf_unix.timestamper in
  MProf.Trace.Control.start trace_config;

  (* Configure logging *)
  Trace_rpc.Helpers.set_reporter ();
  Logs.set_level (Some Logs.Debug);

  Lwt_main.run (IntMap.empty ~directory:"/tmp/irmin/server" () >>= IntMap.start)
