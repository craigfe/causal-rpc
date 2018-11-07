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
    |> IntMap.add "a" 1
    |> IntMap.is_empty
    |> Alcotest.(check bool) "A non-empty map is not empty" false;

    IntMap.empty ~directory:(root ^ "test-0003") ()
    |> IntMap.size
    |> Alcotest.(check int) "The empty map has size 0" 0;

    IntMap.empty ~directory:(root ^ "test-0004") ()
    |> IntMap.add "a" 1
    |> IntMap.add "b" 2
    |> IntMap.size
    |> Alcotest.(check int) "Size behaves reasonably" 2;

    IntMap.empty ~directory:(root ^ "test-0005") ()
    |> IntMap.add "one_hundred" 100
    |> IntMap.mem "one_hundred"
    |> Alcotest.(check bool) "Added values are members of the map" true;

    (IntMap.empty ~directory:(root ^ "test-0006") (), IntMap.empty ~directory:(root ^ "test-0007") ())
    |> (fun (s1, s2) -> (IntMap.add "a" 10 s1, s2))
    |> (fun (s1, s2) -> (IntMap.add "b" 5 s1, s2))
    |> (fun (s1, s2) -> (s1, IntMap.add "a" 1 s2))
    |> (fun (_, s2) -> IntMap.values s2)
    |> Alcotest.(check (list int)) "Maps don't interfere with each other" [1];

    IntMap.empty ~directory:(root ^ "test-0008") ()
    |> IntMap.add "a" 5
    |> IntMap.find "a"
    |> Alcotest.(check int) "Stored bindings can be found" 5;


    IntMap.empty ~directory:(root ^ "test-0009") ()
    |> fun map -> Alcotest.check_raises "Attempting to find a missing binding causes a Not_found exception" Not_found (fun () -> ignore (IntMap.find "not_present" map));


    IntMap.empty ~directory:(root ^ "test-0010") ()
    |> IntMap.add "a" 5
    |> IntMap.add "b" 32
    |> IntMap.add "c" (-15)
    |> IntMap.values
    |> List.sort compare
    |> Alcotest.(check (list int)) "Values are retrieved correctly" [-15; 5; 32];

  end

(** Tests of the distributed increment operation on integer maps *)
let test_increment () =

  (* TODO: Start a worker thread, otherwise this stalls indefinitely! *)

  let open Intmap in begin
    let root = "/tmp/irmin/increment/" in

    IntMap.empty ~directory:(root ^ "test-0001") ()
    |> IntMap.add "a" 1
    |> IntMap.add "b" 10
    |> IntMap.add "c" 100
    |> IntMap.map
    |> IntMap.values
    |> List.sort compare
    |> Alcotest.(check (list int)) "The map increment function works as expected" [2; 11; 101];

  end

let suite = [
	"type", [
	 "base", `Quick, test_base;
   "map", `Quick, test_map;
   (* "increment", `Quick, test_increment; *)
	]
]

let () = Alcotest.run "trace" suite
