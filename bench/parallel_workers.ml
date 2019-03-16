open Lwt.Infix
open Trace_rpc.Intmap

module I = Trace_rpc.Intmap.IntPair(Trace_rpc_unix.Make)(Irmin_unix.Git.FS.G)
include I

let worker ~switch ~batch_size ~thread_count = IntWorker.run
    ~switch
    ~config:(Trace_rpc.Worker.Config.make
               ~batch_size
               ~thread_count
               ~poll_freq:0.01 ())
    ~dir:"/tmp/irmin/bench/threading/worker/"
    ~client:"file:///tmp/irmin/bench/threading/client"
    ()

let maprequest threads () =
  Random.self_init ();
  Trace_rpc.Helpers.set_reporter ();
  Logs.set_level (Some Logs.Debug);
  Printexc.record_backtrace true; (* TODO: remove these *)

  let lwt =
    let value_count = 20 in
    let values = Helpers.sequence_list 1 value_count in
    let keys = List.map Helpers.key_from_int values in

    IntMap.empty ~directory:("/tmp/irmin/bench/threading/client") ()
    >>= IntMap.add_all (Trace_rpc.Helpers.zip keys (List.map Int64.of_int values))
    >>= fun m ->
    let create_worker () = Unix.create_process
        "/home/craigfe/repos/trace-rpc/_build/default/src/trace-rpc-unix/worker/main.exe"
        [|"main"; "file:///tmp/irmin/bench/threading/client"|]
        Unix.stdin
        (* Unix.stdout *)
        (Unix.openfile "/dev/null" [Unix.O_WRONLY] 0o640) (* Redirect output to /dev/null *)
        Unix.stderr

    in

    for _ = 1 to threads do ignore (create_worker ()) done;
    IntMap.map ~timeout:100.0 (O.apply sleep_op 1.) m

    >>= fun _ -> Lwt.return_unit

  in Lwt_main.run lwt


let () =
  (* Configure tracing *)
  let buffer = MProf_unix.mmap_buffer ~size:1000000 "parallel_workers.ctf" in
  let trace_config = MProf.Trace.Control.make buffer MProf_unix.timestamper in
  MProf.Trace.Control.start trace_config;

  maprequest (int_of_string Sys.argv.(1)) ()
