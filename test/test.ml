open Trace_rpc

(** Simple tests of miscellaneous features of the library *)
let test_base () =
  Alcotest.(check string) "The random name function begins with `server--`"
	  (String.sub (Server.random_server_name()) 0 8) "server--"

(** Tests on the IntMap datastructure *)
let test_map () =
  let open Intmap in begin
    let root = "/tmp/irmin/map/" in

    IntMap.empty ~directory:(root ^ "test-0001") ()
    |> IntMap.is_empty
    |> Alcotest.(check bool) "The empty map is empty" true;

    IntMap.empty ~directory:(root ^ "test-0002") ()
    |> IntMap.add "a" Int64.one
    |> IntMap.is_empty
    |> Alcotest.(check bool) "A non-empty map is not empty" false;

    IntMap.empty ~directory:(root ^ "test-0003") ()
    |> IntMap.size
    |> Alcotest.(check int) "The empty map has size 0" 0;

    IntMap.empty ~directory:(root ^ "test-0004") ()
    |> IntMap.add "a" Int64.zero
    |> IntMap.add "b" Int64.one
    |> IntMap.size
    |> Alcotest.(check int) "Size behaves reasonably" 2;

    IntMap.empty ~directory:(root ^ "test-0005") ()
    |> IntMap.add "one" Int64.one
    |> IntMap.mem "one"
    |> Alcotest.(check bool) "Added values are members of the map" true;

    (IntMap.empty ~directory:(root ^ "test-0006") (), IntMap.empty ~directory:(root ^ "test-0007") ())
    |> (fun (s1, s2) -> (IntMap.add "a" (Int64.of_int 10) s1, s2))
    |> (fun (s1, s2) -> (IntMap.add "b" (Int64.of_int 5) s1, s2))
    |> (fun (s1, s2) -> (s1, IntMap.add "a" Int64.one s2))
    |> (fun (_, s2) -> IntMap.values s2)
    |> Alcotest.(check (list int64)) "Maps don't interfere with each other" [Int64.one];

    IntMap.empty ~directory:(root ^ "test-0008") ()
    |> IntMap.add "a" (Int64.of_int 5)
    |> IntMap.find "a"
    |> Alcotest.(check int64) "Stored bindings can be found" (Int64.of_int 5);

    IntMap.empty ~directory:(root ^ "test-0009") ()
    |> fun map -> Alcotest.check_raises "Attempting to find a missing binding causes a Not_found exception" Not_found (fun () -> ignore (IntMap.find "not_present" map));

    IntMap.empty ~directory:(root ^ "test-0010") ()
    |> IntMap.add "a" (Int64.of_int 5)
    |> IntMap.add "b" (Int64.of_int 32)
    |> IntMap.add "c" (Int64.of_int (-15))
    |> IntMap.values
    |> List.map Int64.to_int
    |> List.sort compare
    |> Alcotest.(check (list int)) "Values are retrieved correctly" [-15; 5; 32];

  end

let test_task_queues () =

  let open Intmap in begin
    let root = "/tmp/irmin/task_queues/" in

    IntMap.empty ~directory:(root ^ "test-0001") ()
    |> IntMap.task_queue_is_empty
    |> Alcotest.(check bool) "A new repository has an empty task queue" true;

    IntMap.empty ~directory:(root ^ "test-0002") ()
    |> IntMap.add "a" Int64.one
    |> IntMap.generate_task_queue Definition.double_op Interface.Unit (* TODO: It shouldn't be necessary to pass the empty list here *)

    |> (fun c -> match c with
        | Task_queue (s, []) ->
          (* Mangle the record into nested pairs so that alcotest can check equality *)
          List.map (fun ({name;params;key}:Map.task) -> (name, (params, key))) s
          |> Alcotest.(check (list (pair
                                      Alcotest.pass (* TODO: implement Operation testable *)
                                  (pair (list Type.Param.test_t) string)
                               )))
            "Task queues are generated in the expected format"
            ["", ([],"a")]
        | Task_queue (_, _) -> Alcotest.fail "Generated task queue had finished items"
        | _ -> Alcotest.fail "Generate_task_queue returned a non-task value");

  end

(** Tests of the distributed increment operation on integer maps *)
let test_increment () =
  let open Intmap in begin
    Logs.set_reporter (Logs_fmt.reporter ());
    Logs.set_level (Some Logs.Info);
    let root = "/tmp/irmin/increment/" in

    IntMap.empty ~directory:(root ^ "test-0001") ()
    |> IntMap.map Definition.increment_op Interface.Unit
    |> fun _ -> Alcotest.(check pass "Calling map on an empty Map terminates" () ());

    Alcotest.check_raises "Calling map on a non-empty Map without a worker causes a timeout" Map.Timeout
    (fun () -> ignore (IntMap.empty ~directory:(root ^ "test-0002") ()
    |> IntMap.add "timeout" (Int64.of_int 1)
    |> IntMap.map ~timeout:epsilon_float Definition.increment_op Interface.Unit));

    (* let worker = IntWorker.run ~dir:"/tmp/irmin/increment-worker/test-0001"
     *   ~client:"file:///tmp/irmin/incrmeent/test-0001" () in *)

    (* Lwt_main.run (Lwt.choose [
     *   worker;
     *   (IntMap.empty ~directory:(root ^ "test-0001") ()
     *    |> IntMap.add "a" Int64.one
     *    |> IntMap.map Definition.double_op Interface.Unit (\* TODO: It shouldn't be necessary to pass the empty array here *\)
     *    |> IntMap.find "a"
     *    |> Alcotest.(check int64) "Issuing a double request on a single key" (Int64.of_int 2); Lwt.return_unit)
     * ]); *)

    (* IntMap.empty ~directory:(root ^ "test-0002") ()
     * |> IntMap.add "a" (Int64.of_int 1)
     * |> IntMap.add "b" (Int64.of_int 10)
     * |> IntMap.add "c" (Int64.of_int 100)
     * |> IntMap.map Definition.multiply_op
     *   (Interface.Param (Type.Param.Int64 (Int64.of_int 5), Interface.Unit))
     * |> IntMap.values
     * |> List.map Int64.to_int
     * |> List.sort compare
     * |> Alcotest.(check (list int)) "Issuing a multiply reuqest on several keys" [5; 50; 500]; *)
  end

let suite = [
	"type", [
	  "base",        `Quick, test_base;
    "task_queues", `Quick, test_task_queues;
    "map",         `Quick, test_map;
    "increment",   `Quick, test_increment;
	]
]

let () = Alcotest.run "trace" suite
