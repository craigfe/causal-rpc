let () =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Info);
  Lwt_main.run (Trace_rpc.Intmap.IntWorker.run ~client:Sys.argv.(1) ())
