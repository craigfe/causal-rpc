open Lwt.Infix
open Trace_rpc
open Intmap

module I = IntPair (Trace_rpc_unix.Make)(Helpers.GitBackend)
open I

let worker_pool ?batch_size ?two_phase switch n dir =
  let rec inner n dir =
    match n with
    | 0 -> []
    | n -> let w =
             IntWorker.run
               ~switch
               ~config:(Worker.Config.make
                          ~random_selection:true
                          ?batch_size
                          ?two_phase
                          ~name:("worker_" ^ (string_of_int n))
                          ~poll_freq:0.1 ())
               ~dir:("/tmp/irmin/test_multiple_worker/worker/" ^ dir ^ "/worker_" ^ (string_of_int n))
               ~client:("file:///tmp/irmin/test_multiple_worker/" ^ dir)
               ()
      in w :: inner (n-1) dir
  in inner n dir

let root = "/tmp/irmin/test_multiple_worker/"


let test_single_phase s () =
  let values = Helpers.sequence_list 1 10 in
  let keys = List.map Helpers.key_from_int values in

  IntMap.empty ~directory:(root ^ "single_phase") ()
  >>= IntMap.add_all (Misc.zip keys (List.map Int64.of_int values))
  >>= fun m -> Lwt.pick @@ (worker_pool ~two_phase:false s 4 "single_phase") @ [
      IntMap.map ~timeout:10.0 multiply_op (Interface.Param (Type.int64, Int64.of_int 10, Interface.Unit)) m
      >|= fun _ -> ()
    ]
  >>= fun () -> IntMap.values m
  >|= List.map Int64.to_int
  >|= List.sort compare
  >|= Alcotest.(check (list int)) "Single request on many keys"
    (List.map (fun x -> 10 * x) values)


let test_small_map s () =
  IntMap.empty ~directory:(root ^ "small_map") ()
  >>= IntMap.add_all ["a", Int64.of_int 0]
  >>= fun m -> Lwt.pick @@ (worker_pool s 4 "small_map") @ [
      let rec inner n = match n with
        | 10 -> Lwt.return_unit
        | n ->
          IntMap.map ~timeout:100.0 increment_op Interface.Unit m
          >>= IntMap.values
          >|= List.map Int64.to_int
          >|= Alcotest.(check (list int)) "Many requests on a tiny map" [n]
          >>= fun () -> inner (n + 1)
      in inner 1
    ]


let test_medium_map s () =
  let values = Helpers.sequence_list 1 4 in
  let keys = List.map Helpers.key_from_int values in

  IntMap.empty ~directory:(root ^ "medium_map") ()
  >>= IntMap.add_all (Misc.zip keys (List.map Int64.of_int values))
  >>= fun m -> Lwt.pick @@ (worker_pool s 4 "medium_map") @ [
      let rec inner n = match n with
        | 4 -> Lwt.return_unit
        | n ->
          IntMap.map ~timeout:100.0 increment_op Interface.Unit m
          >>= IntMap.values
          >|= List.map Int64.to_int
          >|= List.sort compare
          >|= Alcotest.(check (list int)) "Series of requests on a medium-sized map"
            (List.map ((+) n) values)

          >>= fun () -> inner (n + 1)
      in inner 1
    ]


let test_large_map s () =
  let values = Helpers.sequence_list 1 12 in
  let keys = List.map Helpers.key_from_int values in

  IntMap.empty ~directory:(root ^ "large_map") ()
  >>= IntMap.add_all (Misc.zip keys (List.map Int64.of_int values))
  >>= fun m -> Lwt.pick @@ (worker_pool s 4 "large_map") @ [
      IntMap.map ~timeout:10.0 multiply_op (Interface.Param (Type.int64, Int64.of_int 10, Interface.Unit)) m
      >|= fun _ -> ()
    ]
  >>= fun () -> IntMap.values m
  >|= List.map Int64.to_int
  >|= List.sort compare
  >|= Alcotest.(check (list int)) "Single request on many keys"
    (List.map (fun x -> 10 * x) values)


let test_batched_work s () =
  let values = Helpers.sequence_list 1 12 in
  let keys = List.map Helpers.key_from_int values in

  IntMap.empty ~directory:(root ^ "batched_work") ()
  >>= IntMap.add_all (Misc.zip keys (List.map Int64.of_int values))
  >>= fun m -> Lwt.pick @@ (worker_pool ~batch_size:4 s 4 "batched_work") @ [
      IntMap.map ~timeout:10.0 multiply_op (Interface.Param (Type.int64, Int64.of_int 10, Interface.Unit)) m
      >|= fun _ -> ()
    ]
  >>= fun () -> IntMap.values m
  >|= List.map Int64.to_int
  >|= List.sort compare
  >|= Alcotest.(check (list int)) "Single request on many keys"
    (List.map (fun x -> 10 * x) values)


let tests = [
  Alcotest_lwt.test_case "Single-phase workers" `Slow test_single_phase;
  Alcotest_lwt.test_case "Small map" `Slow test_small_map;
  Alcotest_lwt.test_case "Medium-sized map" `Slow test_medium_map;
  Alcotest_lwt.test_case "Large map" `Slow test_large_map;
  Alcotest_lwt.test_case "Batched work" `Slow test_batched_work;
]
