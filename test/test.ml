let suite = [
	"type", [
	  "misc",        `Quick, Test_misc.test;
    "task_queues", `Quick, Test_task_queues.test;
    "map",         `Quick, Test_map.test;
    "increment",   `Quick, Test_increment.test;
	]
]

let () = Alcotest.run "trace" suite
