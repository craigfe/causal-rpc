let suite = [
  "map", [
    "misc",        `Quick, Test_misc.test;
    Alcotest_lwt.test_case "task_queues" `Quick Test_task_queues.test;
 ];
 "Local map ops", Test_map.tests;
 "Concurrent map ops", Test_increment.tests;
 "Stress testing", Test_stress.tests
]

let () =
  Random.self_init ();
  Trace_rpc.Misc.set_reporter ();
  Logs.set_level (Some Logs.Info);

  Alcotest.run "trace" suite
