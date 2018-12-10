open Lwt.Infix
open Trace_rpc
open Intmap

let worker switch dir = IntWorker.run
    ~switch
    ~dir:("/tmp/irmin/test_stress/worker/" ^ dir)
    ~client:("file:///tmp/irmin/test_stress/" ^ dir)
    ~poll_freq:0.01 ()

let worker_pool switch n dir =
  let rec inner n dir =
    match n with
    | 0 -> []
    | n -> let w =
             IntWorker.run
               ~random_selection:true
               ~switch
               ~name:("worker_" ^ (string_of_int n))
               ~dir:("/tmp/irmin/test_stress/worker/" ^ dir ^ "/worker_" ^ (string_of_int n))
               ~client:("file:///tmp/irmin/test_stress/" ^ dir)
               ~poll_freq:0.01 ()
      in w :: inner (n-1) dir
  in inner n dir

let stress_test s () =
  Misc.set_reporter ();
  Logs.set_level (Some Logs.Info);
  let root = "/tmp/irmin/test_stress/stress/" in
  let original_values = [1;2;3;4;5;6;7;8;9;10;11;12] in
  let () = Lwt_preemptive.simple_init () in

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
  >>= fun m -> Lwt.pick @@ (worker_pool s 4 "stress/test-0001") @ [
    let rec inner n map =
      IntMap.map ~timeout:100.0 increment_op Interface.Unit map
      >>= IntMap.values
      >|= List.map Int64.to_int
      >|= List.sort compare
      >>= fun actual -> Lwt.return (List.map ((+) n) original_values)
      >>= fun expected -> Lwt.return (Alcotest.(check (list int)) "Multiple request on many keys concurrently" expected actual)
      >>= fun () -> inner (n + 1) map
    in inner 1 m
  ]

let tests = [

  Alcotest_lwt.test_case "Stress test" `Slow stress_test
]
