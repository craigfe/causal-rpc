open Lwt.Infix
open Trace_rpc

(** Tests of the distributed increment operation on integer maps *)
let basic_tests () =
  let open Intmap in begin
    Logs.set_reporter (Logs_fmt.reporter ());
    Logs.set_level (Some Logs.Info);
    let root = "/tmp/irmin/increment/" in

    IntMap.empty ~directory:(root ^ "test-0001") ()
    |> IntMap.map increment_op Interface.Unit
    |> fun _ -> Alcotest.(check pass "Calling map on an empty Map terminates" () ());

    Alcotest.check_raises "Calling map on a non-empty Map without a worker causes a timeout" Map.Timeout
    (fun () -> ignore (IntMap.empty ~directory:(root ^ "test-0002") ()
    |> IntMap.add "timeout" (Int64.of_int 1)
    |> IntMap.map ~timeout:epsilon_float increment_op Interface.Unit))
  end


let mapfn _ () =
  let open Intmap in begin
    Logs.set_reporter (Logs_fmt.reporter ());
    Logs.set_level (Some Logs.Info);
    let root = "/tmp/irmin/increment-mapfn/" in
    (* let worker dir = IntWorker.run ~dir:("/tmp/irmin/increment-mapfn-worker/" ^ dir)
     *     ~client:("file:///tmp/irmin/increment-mapfn/" ^ dir) () in *)

    (* TODO: remove this *)
    (* IntMap.empty ~directory:(root ^ "test-0003") ()
     * |> IntMap.add "a" Int64.one
     * |> IntMap.map ~timeout:1000.0 increment_op Interface.Unit
     * |> IntMap.find "a"
     * |> Alcotest.(check int64) "Issuing a double request on a single key" (Int64.of_int 2); *)

    Lwt_preemptive.simple_init ();

    Lwt.pick [
        worker "test-0001";

        IntMap.empty ~directory:(root ^ "test-0001") ()
         |> IntMap.add "a" Int64.one
         |> IntMap.map ~timeout:5.0 identity_op Interface.Unit (* TODO: It shouldn't be necessary to pass the empty array here *)
         >|= IntMap.find "a"
         >|= Alcotest.(check int64) "Issuing a no-op request on a single key" Int64.one
      ]

    (* IntMap.empty ~directory:(root ^ "test-0003") ()
     * |> IntMap.add "a" (Int64.of_int 1)
     * |> IntMap.add "b" (Int64.of_int 10)
     * |> IntMap.add "c" (Int64.of_int 100)
     * |> IntMap.map ~timeout:1000.0 multiply_op
     *   (Interface.Param (Type.int64, Int64.of_int 5, Interface.Unit))
     * |> IntMap.values
     * |> List.map Int64.to_int
     * |> List.sort compare
     * |> Alcotest.(check (list int)) "Issuing a multiply reuqest on several keys" [5; 50; 500]; *)
  end


let tests = [
  (* "basic", `Quick, basic_tests; *)
  "mapfn", `Quick, mapfn
]

