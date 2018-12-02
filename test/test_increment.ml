open Lwt.Infix
open Trace_rpc
open Intmap

let worker dir = IntWorker.run
    ~dir:("/tmp/irmin/test_increment/worker/" ^ dir)
    ~client:("file:///tmp/irmin/test_increment/" ^ dir)
    ~poll_freq:0.0001 ()

let worker_pool n dir =
  let rec inner n dir =
    match n with
    | 0 -> []
    | n -> let w = IntWorker.run
               ~dir:("/tmp/irmin/test_increment/worker/" ^ dir ^ "/" ^ (string_of_int n))
               ~client:("file:///tmp/irmin/test_increment/" ^ dir)
               ~poll_freq:epsilon_float ()

      in w :: inner (n-1) dir
  in Lwt.pick (inner n dir)

(** Tests of the distributed increment operation on integer maps *)
let basic_tests _ () =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Info);
  let root = "/tmp/irmin/increment/" in

  IntMap.empty ~directory:(root ^ "test-0001") ()
  |> IntMap.map increment_op Interface.Unit
  >|= (fun _ -> Alcotest.(check pass "Calling map on an empty Map terminates" () ()))


let timeout_tests () =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Info);
  let root = "/tmp/irmin/timeout/" in
  let descr = "Calling map on a non-empty Map without a worker causes a timeout" in

  try Lwt_main.run (
      IntMap.empty ~directory:(root ^ "test-0001") ()
      |> IntMap.add "unchanged" Int64.one
      |> IntMap.map ~timeout:epsilon_float increment_op Interface.Unit
      >|= fun _ -> Alcotest.(fail descr))

  with Map.Timeout -> Alcotest.(check pass descr Map.Timeout Map.Timeout)


(** Tests of the scheduling infrastructure and workers, using noops to do 'work' *)
let noop_tests _ () =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Info);
  let root = "/tmp/irmin/test_increment/noop/" in

  Lwt_preemptive.simple_init ();

  Lwt.pick [
    worker "noop/test-0001";

    IntMap.empty ~directory:(root ^ "test-0001") ()
    |> IntMap.add "a" Int64.one
    |> IntMap.map identity_op Interface.Unit
    >|= IntMap.find "a"
    >|= Alcotest.(check int64) "No-op request on a single key" Int64.one
  ] >>= fun () ->

  Lwt.pick [
    worker "noop/test-0002";

    IntMap.empty ~directory:(root ^ "test-0002") ()
    |> IntMap.add "a" Int64.one
    |> IntMap.map ~timeout:5.0 identity_op Interface.Unit
    >>= IntMap.map ~timeout:5.0 identity_op Interface.Unit
    >>= IntMap.map ~timeout:5.0 identity_op Interface.Unit

    >|= IntMap.find "a"
    >|= Alcotest.(check int64) "Multiple no-op requests on a single key in series" Int64.one
  ]

  (* >>= fun () ->
   * 
   * Lwt.pick [
   *   worker_pool 4 "noop/test-0003";
   * 
   *   IntMap.empty ~directory:(root ^ "test-0003") ()
   *   |> IntMap.add "a" (Int64.of_int 1)
   *   |> IntMap.add "b" (Int64.of_int 2)
   *   |> IntMap.add "c" (Int64.of_int 3)
   *   |> IntMap.add "d" (Int64.of_int 4)
   *   (\* |> IntMap.add "e" (Int64.of_int 5)
   *    * |> IntMap.add "f" (Int64.of_int 6)
   *    * |> IntMap.add "g" (Int64.of_int 7)
   *    * |> IntMap.add "h" (Int64.of_int 8) *\)
   * 
   *   |> IntMap.map ~timeout:5.0 identity_op Interface.Unit
   * 
   *   >|= IntMap.find "a"
   *   >|= Alcotest.(check int64) "No-op request on many keys in parallel" Int64.one
   * ] *)


let increment_tests _ () =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Info);
  Lwt_preemptive.simple_init ();
  let root = "/tmp/irmin/test_increment/increment/" in

  Lwt.pick [
    worker "increment/test-0001";

    IntMap.empty ~directory:(root ^ "test-0001") ()
    |> IntMap.add "a" Int64.zero
    |> IntMap.map increment_op Interface.Unit
    >|= IntMap.find "a"
    >|= Alcotest.(check int64) "Increment request on a single key" Int64.one
  ] >>= fun () ->

  Lwt.pick [
    worker "increment/test-0002";

    IntMap.empty ~directory:(root ^ "test-0002") ()
    |> IntMap.add "a" Int64.zero
    |> IntMap.map increment_op Interface.Unit
    >>= IntMap.map increment_op Interface.Unit
    >>= IntMap.map increment_op Interface.Unit
    >|= IntMap.find "a"
    >|= Alcotest.(check int64) "Multiple increment requests on a single key in series" (Int64.of_int 3)
  ] >>= fun () ->

  Lwt.pick [
    worker "increment/test-0003";

    IntMap.empty ~directory:(root ^ "test-0003") ()
    |> IntMap.add "a" (Int64.of_int 0)
    |> IntMap.add "b" (Int64.of_int 10)
    |> IntMap.add "c" (Int64.of_int 100)
    |> IntMap.map increment_op Interface.Unit
    >|= IntMap.values
    >|= List.map Int64.to_int
    >|= List.sort compare
    >|= Alcotest.(check (list int)) "Increment request on multiple keys" [1; 11; 101]
  ]

let multiply_tests _ () =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Info);
  Lwt_preemptive.simple_init ();
  let root = "/tmp/irmin/test_increment/multiply/" in

  Lwt.pick [
    worker "multiply/test-0001";

    IntMap.empty ~directory:(root ^ "test-0001") ()
    |> IntMap.add "a" (Int64.of_int 0)
    |> IntMap.add "b" (Int64.of_int 10)
    |> IntMap.add "c" (Int64.of_int 100)
    |> IntMap.map multiply_op (Interface.Param (Type.int64, Int64.of_int 5, Interface.Unit))
    >|= IntMap.values
    >|= List.map Int64.to_int
    >|= List.sort compare
    >|= Alcotest.(check (list int)) "Multiply request on multiple keys" [0; 50; 500]
  ]

let tests = [
  Alcotest_lwt.test_case "Workerless tests" `Quick basic_tests;
  "Tests of timeouts", `Quick, timeout_tests;
  Alcotest_lwt.test_case "No-op testing" `Quick noop_tests;
  Alcotest_lwt.test_case "Increment testing" `Quick increment_tests;
  Alcotest_lwt.test_case "Multiply testing" `Quick multiply_tests
]

