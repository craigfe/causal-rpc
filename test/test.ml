let suite = [
	"map", [
	  "misc",        `Quick, Test_misc.test;
    "task_queues", `Quick, Test_task_queues.test;
 ];
 "Local map ops", Test_map.tests;
 "Distributed map ops", Test_increment.tests
]

let () = Alcotest.run "trace" suite
