open Lwt.Infix
open Trace_rpc
open Intmap

module I = IntPair (Trace_rpc_unix.Make)(Global.GitBackend)
open I

let worker ?batch_size switch dir = IntWorker.run
    ~switch
    ~config:(Worker.Config.make
       ?batch_size
       ~poll_freq:0.01 ())
    ~dir:("/tmp/irmin/test_single_worker/worker/" ^ dir)
    ~client:("file:///tmp/irmin/test_single_worker/" ^ dir)
    ()

let worker_pool switch n dir =
  let rec inner n dir =
    match n with
    | 0 -> []
    | n -> let w =
             IntWorker.run
               ~switch
               ~config:(Worker.Config.make
                          ~name:("worker_" ^ (string_of_int n))
                          ~poll_freq:0.01 ())
               ~dir:("/tmp/irmin/test_single_worker/worker/" ^ dir ^ "/worker_" ^ (string_of_int n))
               ~client:("file:///tmp/irmin/test_single_worker/" ^ dir)
               ()
      in w :: inner (n-1) dir
  in inner n dir

let basic_tests _ () =
  let root = "/tmp/irmin/increment/" in

  IntMap.empty ~directory:(root ^ "test-0001") ()
  >>= IntMap.map increment_op Operation.Unit
  >|= (fun _ -> Alcotest.(check pass "Calling map on an empty Map terminates" () ()))

let timeout_tests () =
  let root = "/tmp/irmin/timeout/" in
  let descr = "Calling map on a non-empty Map without a worker causes a timeout" in

  try Lwt_main.run (
      IntMap.empty ~directory:(root ^ "test-0001") ()
      >>= IntMap.add "unchanged" Int64.one
      >>= IntMap.map ~timeout:epsilon_float increment_op Operation.Unit
      >|= fun _ -> Alcotest.(fail descr))

  with Exceptions.Timeout -> Alcotest.(check pass descr Exceptions.Timeout Exceptions.Timeout)

(** Tests of the scheduling infrastructure and workers, using noops to do 'work' *)
let noop_tests s () =
  let open IntMap in
  let root = "/tmp/irmin/test_single_worker/noop/" in

  empty ~directory:(root ^ "test-0001") ()
  >>= add "a" Int64.one
  >>= fun m -> Lwt.pick [
    worker s "noop/test-0001";

    map identity_op Operation.Unit m
    >|= fun _ -> ()
  ]

  >>= fun () -> find "a" m
  >|= Alcotest.(check int64) "No-op request on a single key" Int64.one

  >>= fun () -> empty ~directory:(root ^ "test-0002") ()
  >>= add "a" Int64.one
  >>= fun m -> Lwt.pick [
    worker s "noop/test-0002";

    map ~timeout:5.0 identity_op Operation.Unit m
    >>= map ~timeout:5.0 identity_op Operation.Unit
    >>= map ~timeout:5.0 identity_op Operation.Unit
    >|= fun _ -> ()
  ]
  >>= fun _ -> find "a" m
  >|= Alcotest.(check int64) "Multiple no-op requests on a single key in series" Int64.one

let increment_tests s () =
  let open IntMap in
  let root = "/tmp/irmin/test_single_worker/increment/" in

  empty ~directory:(root ^ "test-0001") ()
  >>= add "a" Int64.zero
  >>= fun m -> Lwt.pick [
    worker s "increment/test-0001";

    map increment_op Operation.Unit m
    >|= fun _ -> ()
  ]
  >>= fun () -> find "a" m
  >|= Alcotest.(check int64) "Increment request on a single key" Int64.one


  >>= fun () -> empty ~directory:(root ^ "test-0002") ()
  >>= add "a" Int64.zero
  >>= fun m -> Lwt.pick [
    worker s "increment/test-0002";

    map increment_op Operation.Unit m
    >>= map increment_op Operation.Unit
    >>= map increment_op Operation.Unit
    >|= fun _ -> ()
  ]
  >>= fun () -> find "a" m
  >|= Alcotest.(check int64) "Multiple increment requests on a single key in series" (Int64.of_int 3)

  >>= empty ~directory:(root ^ "test-0003")
  >>= add "a" (Int64.of_int 0)
  >>= add "b" (Int64.of_int 10)
  >>= add "c" (Int64.of_int 100)
  >>= fun m -> Lwt.pick [
    worker s "increment/test-0003";

    map increment_op Operation.Unit m
    >|= fun _ -> ()
  ]
  >>= fun () -> values m
  >|= List.map Int64.to_int
  >|= List.sort compare
  >|= Alcotest.(check (list int)) "Increment request on multiple keys" [1; 11; 101]

let multiply_tests s () =
  let root = "/tmp/irmin/test_single_worker/multiply/" in

  IntMap.empty ~directory:(root ^ "test-0001") ()
  >>= IntMap.add "a" (Int64.of_int 0)
  >>= IntMap.add "b" (Int64.of_int 10)
  >>= IntMap.add "c" (Int64.of_int 100)
  >>= fun m -> Lwt.pick [
    worker s "multiply/test-0001";

    IntMap.map multiply_op (Operation.Param (Type.int64, Int64.of_int 5, Operation.Unit)) m
    >|= fun _ -> ()
  ]
  >>= fun () -> IntMap.values m
  >|= List.map Int64.to_int
  >|= List.sort compare
  >|= Alcotest.(check (list int)) "Multiply request on multiple keys" [0; 50; 500]

let test_work_batches s () =
  let root = "/tmp/irmin/test_single_worker/work_batches/" in

  IntMap.empty ~directory:(root ^ "test-0001") ()
  >>= IntMap.add_all ["a", Int64.of_int 0;
                      "b", Int64.of_int 1;
                      "c", Int64.of_int 2;
                      "d", Int64.of_int 3;
                      "e", Int64.of_int 4]

  >>= fun m -> Lwt.pick [
    worker ~batch_size:2 s "work_batches/test-0001";
    IntMap.map multiply_op (Operation.Param (Type.int64, Int64.of_int 10, Operation.Unit)) m
    >|= fun _ -> ()
  ]
  >>= fun () -> IntMap.values m
  >|= List.map Int64.to_int
  >|= List.sort compare
  >|= Alcotest.(check (list int)) "Batch size of 2 on a map of size 5" [0; 10; 20; 30; 40]

  >>= IntMap.empty ~directory:(root ^ "test-0002")
  >>= IntMap.add_all ["a", Int64.of_int 0;
                      "b", Int64.of_int 1;
                      "c", Int64.of_int 2;
                      "d", Int64.of_int 3;
                      "e", Int64.of_int 4]

  >>= fun m -> Lwt.pick [
    worker ~batch_size:10 s "work_batches/test-0002";
    IntMap.map multiply_op (Operation.Param (Type.int64, Int64.of_int 10, Operation.Unit)) m
    >|= fun _ -> ()
  ]
  >>= fun () -> IntMap.values m
  >|= List.map Int64.to_int
  >|= List.sort compare
  >|= Alcotest.(check (list int)) "Batch size of 10 on a map of size 5" [0; 10; 20; 30; 40]

let tests = [
  Alcotest_lwt.test_case "Workerless tests" `Quick basic_tests;
  "Tests of timeouts", `Quick, timeout_tests;
  Alcotest_lwt.test_case "No-op operations" `Quick noop_tests;
  Alcotest_lwt.test_case "Increment operations" `Quick increment_tests;
  Alcotest_lwt.test_case "Multiply operations" `Quick multiply_tests;
  Alcotest_lwt.test_case "Batched work" `Quick test_work_batches;
]

