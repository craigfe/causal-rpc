let suite = [
	"map", [
	  "misc",        `Quick, Test_misc.test;
    "task_queues", `Quick, Test_task_queues.test;
    "map",         `Quick, Test_map.test;
 ];
 "increment", Test_increment.tests
]

let () = Alcotest.run "trace" suite
