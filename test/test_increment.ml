open Lwt.Infix
open Trace_rpc
open Intmap

let worker dir = IntWorker.run
    ~dir:("/tmp/irmin/test_increment/worker/" ^ dir)
    ~client:("file:///tmp/irmin/test_increment/" ^ dir)
    ~poll_freq:0.01 ()

let worker_pool n dir =
  let rec inner n dir =
    match n with
    | 0 -> []
    | n -> let w =
             IntWorker.run
               ~name:("worker_" ^ (string_of_int n))
               ~dir:("/tmp/irmin/test_increment/worker/" ^ dir ^ "/worker_" ^ (string_of_int n))
               ~client:("file:///tmp/irmin/test_increment/" ^ dir)
               ~poll_freq:0.01 ()
      in w :: inner (n-1) dir
  in inner n dir

(** Tests of the distributed increment operation on integer maps *)
let basic_tests _ () =
  Misc.set_reporter ();
  Logs.set_level (Some Logs.Info);
  let root = "/tmp/irmin/increment/" in

  IntMap.empty ~directory:(root ^ "test-0001") ()
  >>= IntMap.map increment_op Interface.Unit
  >|= (fun _ -> Alcotest.(check pass "Calling map on an empty Map terminates" () ()))


let timeout_tests () =
  Misc.set_reporter ();
  Logs.set_level (Some Logs.Info);
  let root = "/tmp/irmin/timeout/" in
  let descr = "Calling map on a non-empty Map without a worker causes a timeout" in

  try Lwt_main.run (
      IntMap.empty ~directory:(root ^ "test-0001") ()
      >>= IntMap.add "unchanged" Int64.one
      >>= IntMap.map ~timeout:epsilon_float increment_op Interface.Unit
      >|= fun _ -> Alcotest.(fail descr))

  with Map.Timeout -> Alcotest.(check pass descr Map.Timeout Map.Timeout)


(** Tests of the scheduling infrastructure and workers, using noops to do 'work' *)
let noop_tests _ () =
  let open IntMap in

  Misc.set_reporter ();
  Logs.set_level (Some Logs.Info);
  let root = "/tmp/irmin/test_increment/noop/" in

  empty ~directory:(root ^ "test-0001") ()
  >>= add "a" Int64.one
  >>= fun m -> Lwt.pick [
    worker "noop/test-0001";

    map identity_op Interface.Unit m
    >|= fun _ -> ()
  ]

  >>= fun () -> find "a" m
  >|= Alcotest.(check int64) "No-op request on a single key" Int64.one

  >>= fun () -> empty ~directory:(root ^ "test-0002") ()
  >>= add "a" Int64.one
  >>= fun m -> Lwt.pick [
    worker "noop/test-0002";

    map ~timeout:5.0 identity_op Interface.Unit m
    >>= map ~timeout:5.0 identity_op Interface.Unit
    >>= map ~timeout:5.0 identity_op Interface.Unit
    >|= fun _ -> ()
  ]
  >>= fun _ -> find "a" m
  >|= Alcotest.(check int64) "Multiple no-op requests on a single key in series" Int64.one

let increment_tests _ () =
  let open IntMap in

  Misc.set_reporter ();
  Logs.set_level (Some Logs.Info);
  let root = "/tmp/irmin/test_increment/increment/" in

  empty ~directory:(root ^ "test-0001") ()
  >>= add "a" Int64.zero
  >>= fun m -> Lwt.pick [
    worker "increment/test-0001";

    map increment_op Interface.Unit m
    >|= fun _ -> ()
  ]
  >>= fun () -> find "a" m
  >|= Alcotest.(check int64) "Increment request on a single key" Int64.one


  >>= fun () -> empty ~directory:(root ^ "test-0002") ()
  >>= add "a" Int64.zero
  >>= fun m -> Lwt.pick [
    worker "increment/test-0002";

    map increment_op Interface.Unit m
    >>= map increment_op Interface.Unit
    >>= map increment_op Interface.Unit
    >|= fun _ -> ()
  ]
  >>= fun () -> find "a" m
  >|= Alcotest.(check int64) "Multiple increment requests on a single key in series" (Int64.of_int 3)

  >>= empty ~directory:(root ^ "test-0003")
  >>= add "a" (Int64.of_int 0)
  >>= add "b" (Int64.of_int 10)
  >>= add "c" (Int64.of_int 100)
  >>= fun m -> Lwt.pick [
    worker "increment/test-0003";

    map increment_op Interface.Unit m
    >|= fun _ -> ()
  ]
  >>= fun () -> values m
  >|= List.map Int64.to_int
  >|= List.sort compare
  >|= Alcotest.(check (list int)) "Increment request on multiple keys" [1; 11; 101]

let multiply_tests _ () =
  Misc.set_reporter ();
  Logs.set_level (Some Logs.Info);
  let root = "/tmp/irmin/test_increment/multiply/" in

  IntMap.empty ~directory:(root ^ "test-0001") ()
  >>= IntMap.add "a" (Int64.of_int 0)
  >>= IntMap.add "b" (Int64.of_int 10)
  >>= IntMap.add "c" (Int64.of_int 100)
  >>= fun m -> Lwt.pick [
    worker "multiply/test-0001";

    IntMap.map multiply_op (Interface.Param (Type.int64, Int64.of_int 5, Interface.Unit)) m
    >|= fun _ -> ()
  ]
  >>= fun () -> IntMap.values m
  >|= List.map Int64.to_int
  >|= List.sort compare
  >|= Alcotest.(check (list int)) "Multiply request on multiple keys" [0; 50; 500]


let worker_pool_tests _ () =
  Misc.set_reporter ();
  Logs.set_level (Some Logs.Info);
  let root = "/tmp/irmin/test_increment/worker_pool/" in

  IntMap.empty ~directory:(root ^ "test-0001") ()
  >>= IntMap.add_all
    ["a", Int64.of_int 1;
     "b", Int64.of_int 2;
     "c", Int64.of_int 3;
     "d", Int64.of_int 4;
     "e", Int64.of_int 5;
     "f", Int64.of_int 6;
     "g", Int64.of_int 7;
     "h", Int64.of_int 8;
     "i", Int64.of_int 9;
     "j", Int64.of_int 10;
     "k", Int64.of_int 11;
     "l", Int64.of_int 12]
  >>= fun m -> Lwt.pick @@ (worker_pool 4 "worker_pool/test-0001") @ [
      IntMap.map ~timeout:5.0 multiply_op (Interface.Param (Type.int64, Int64.of_int 10, Interface.Unit)) m
      >|= fun _ -> ()
    ]
  >>= fun () -> IntMap.values m
  >|= List.map Int64.to_int
  >|= List.sort compare
  >|= Alcotest.(check (list int)) "Multiple request on many keys concurrently"
    [10; 20; 30; 40; 50; 60; 70; 80; 90; 100; 110; 120]


let tests = [
  Alcotest_lwt.test_case "Workerless tests" `Quick basic_tests;
  "Tests of timeouts", `Quick, timeout_tests;
  Alcotest_lwt.test_case "No-op testing" `Quick noop_tests;
  Alcotest_lwt.test_case "Increment testing" `Quick increment_tests;
  Alcotest_lwt.test_case "Multiply testing" `Quick multiply_tests;
  Alcotest_lwt.test_case "Worker pool testing" `Slow worker_pool_tests;
  (* Alcotest_lwt.test_case "Parallel testing" `Quick parallel_tests *)
]

