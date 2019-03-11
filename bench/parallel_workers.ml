open Intmap
open Lwt.Infix

let worker ~switch ~batch_size ~thread_count = IntWorker.run
    ~switch
    ~config:(Trace_rpc.Worker.Config.make
               ~batch_size
               ~thread_count
               ~poll_freq:0.01 ())
    ~dir:"/tmp/irmin/bench/threading/worker/"
    ~client:"file:///tmp/irmin/bench/threading/client"
    ()

let maprequest _threads () =
  Random.self_init ();
  Trace_rpc.Misc.set_reporter ();
  Logs.set_level (Some Logs.Debug);
  Printexc.record_backtrace true; (* TODO: remove these *)

  let lwt =
    let value_count = 100 in
    let values = Helpers.sequence_list 1 value_count in
    let keys = List.map Helpers.key_from_int values in

    IntMap.empty ~directory:("/tmp/irmin/bench/threading/client") ()
    >>= IntMap.add_all (Trace_rpc.Misc.zip keys (List.map Int64.of_int values))
    >>= fun m -> let _ = Unix.create_process
                     "/home/craigfe/repos/trace-rpc/_build/default/src/trace-rpc-unix/worker/main.exe"
                     [|"main"; "file:///tmp/irmin/bench/threading/client"|]
                     Unix.stdin
                     Unix.stdout
                     Unix.stderr
    in

    IntMap.map ~timeout:100.0 Trace_rpc.Intmap.sleep_op (Trace_rpc.Interface.Param (Trace_rpc.Type.float, 0.001, Trace_rpc.Interface.Unit)) m

    >>= fun _ -> Lwt.return_unit

  in Lwt_main.run lwt


let () =
  maprequest (int_of_string Sys.argv.(1)) ()
