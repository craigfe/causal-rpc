let suite = [
  "Miscellaneous", Test_misc.tests;
  "Task queues", Test_task_queues.tests;
  "One-to-one unicast RPC", Test_unicast.tests;
  "Local map ops", Test_map.tests;
  "Single-worker maps", Test_single_worker.tests;
  (* "Multi-threaded worker maps", Test_parallel_worker.tests; *)
  "Multiple-worker maps", Test_multiple_worker.tests;
  (* "Stress testing", Test_stress.tests *)
]

let () =
  Random.self_init ();
  Trace_rpc.Misc.set_reporter ();
  Logs.set_level (Some Logs.Info);
  Printexc.record_backtrace true;

  Alcotest.run "trace" suite
