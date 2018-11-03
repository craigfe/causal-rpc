let () =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Debug);
  Lwt_main.run (Trace_rpc.Worker.run ~client:Sys.argv.(1) ())
