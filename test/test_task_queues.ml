open Trace_rpc

let test () =

  let open Intmap in begin
    let root = "/tmp/irmin/task_queues/" in

    IntMap.empty ~directory:(root ^ "test-0001") ()
    |> IntMap.task_queue_is_empty
    |> Alcotest.(check bool) "A new repository has an empty task queue" true;

    IntMap.empty ~directory:(root ^ "test-0002") ()
    |> IntMap.add "a" Int64.one
    |> IntMap.generate_task_queue Definition.increment_op Interface.Unit (* TODO: It shouldn't be necessary to pass the empty list here *)

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
