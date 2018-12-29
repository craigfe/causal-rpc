open Lwt.Infix
open Trace_rpc
open Intmap

module I = IntPair (Trace_rpc_unix.Make)(Irmin_unix.Git.FS.G)
open I

(** Tests on the IntMap datastructure *)
let test _ () =
  let root = "/tmp/irmin/map/" in

  IntMap.empty ~directory:(root ^ "test-0001") ()
  >>= IntMap.is_empty
  >|= Alcotest.(check bool) "The empty map is empty" true

  >>= IntMap.empty ~directory:(root ^ "test-0002")
  >>= IntMap.add "a" Int64.one
  >>= IntMap.is_empty
  >|= Alcotest.(check bool) "A non-empty map is not empty" false

  >>= IntMap.empty ~directory:(root ^ "test-0003")
  >>= IntMap.size
  >|= Alcotest.(check int) "The empty map has size 0" 0

  >>= IntMap.empty ~directory:(root ^ "test-0004")
  >>= IntMap.add "a" Int64.zero
  >>= IntMap.add "b" Int64.one
  >>= IntMap.size
  >|= Alcotest.(check int) "Size behaves reasonably" 2

  >>= IntMap.empty ~directory:(root ^ "test-0005")
  >>= IntMap.add "one" Int64.one
  >>= IntMap.mem "one"
  >|= Alcotest.(check bool) "Added values are members of the map" true

  >>= IntMap.empty ~directory:(root ^ "test-0006")
  >>= fun s1 -> IntMap.empty ~directory:(root ^ "test-0007") ()
  >>= fun s2 -> IntMap.add "a" (Int64.of_int 10) s1
  >>= fun _ -> IntMap.add "b" (Int64.of_int 5) s1
  >>= fun _ -> IntMap.add "a" Int64.one s2
  >>= fun _ -> IntMap.values s2
  >|= Alcotest.(check (list int64)) "Maps don't interfere with each other" [Int64.one]

  >>= IntMap.empty ~directory:(root ^ "test-0008")
  >>= IntMap.add "a" (Int64.of_int 5)
  >>= IntMap.find "a"
  >|= Alcotest.(check int64) "Stored bindings can be found" (Int64.of_int 5)

  (* >>= IntMap.empty ~directory:(root ^ "test-0009")
   * >|= (fun map -> Alcotest.check_raises "Attempting to find a missing binding causes a Not_found exception" Not_found (fun () -> ignore (IntMap.find "not_present" map))) *)

  >>= IntMap.empty ~directory:(root ^ "test-0010")
  >>= IntMap.add "a" (Int64.of_int 5)
  >>= IntMap.add "b" (Int64.of_int 32)
  >>= IntMap.add "c" (Int64.of_int (-15))
  >>= IntMap.values
  >|= List.map Int64.to_int
  >|= List.sort compare
  >|= Alcotest.(check (list int)) "Values are retrieved correctly" [-15; 5; 32]


let tests = [
  Alcotest_lwt.test_case "Basic" `Quick test
]
