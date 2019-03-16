open Lwt.Infix
open Trace_rpc
open Intmap

module GitBackend = Irmin_unix.Git.FS.G
module I = Intmap.IntPair (Trace_rpc_unix.Make)(GitBackend)
open I

let create_client directory remote =
  IntClient.empty ~directory
    ~local_uri:("file://" ^ directory)
    ~remote_uri:("file://" ^ remote)
    ~name:"client"
    ~initial:Int64.one

let () =
  (* Configure tracing *)
  let buffer = MProf_unix.mmap_buffer ~size:1000000 "client.ctf" in
  let trace_config = MProf.Trace.Control.make buffer MProf_unix.timestamper in
  MProf.Trace.Control.start trace_config;

  (* Configure logging *)
  Trace_rpc.Helpers.set_reporter ();
  Logs.set_level (Some Logs.Debug);

  let lwt =
    create_client "/tmp/irmin/client" "/tmp/irmin/server"
    >>= fun client -> IntClient.rpc (O.apply increment_op) client
    >|= fun v -> print_string @@ Fmt.strf "Got value %Ld" v
  in

  Lwt_main.run lwt
