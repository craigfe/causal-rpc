let () =
  Trace_rpc.Helpers.set_reporter ();
  Logs.set_level (Some Logs.Debug);

  let dir = "/tmp/irmin/worktest" in
  let client = "file:///tmp/irmin/test_stress/stress" in

  let config = Trace_rpc.Worker.Config.make
      ~random_selection:true
      ~batch_size:1
      ~thread_count:1
      ~name:"worktest"
      ~poll_freq:0.01
      ~two_phase:false
      ()
  in

  Lwt_main.run @@ Intmap.IntWorker.run ~config ~dir ~client ()
