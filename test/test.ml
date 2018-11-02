open Trace_rpc

(** Tests on the IntMap datastructure *)
let test_set () =
  let open Intmap in begin
    let root = "/tmp/irmin/set/" in

    IntMap.empty ~directory:(root ^ "test-0001") ()
    |> IntMap.is_empty
    |> Alcotest.(check bool) "The empty set is empty" true;

    IntMap.empty ~directory:(root ^ "test-0002") ()
    |> IntMap.add "a" 1
    |> IntMap.is_empty
    |> Alcotest.(check bool) "A non-empty set is not empty" false;

    IntMap.empty ~directory:(root ^ "test-0003") ()
    |> IntMap.size
    |> Alcotest.(check int) "The empty set has size 0" 0;

    IntMap.empty ~directory:(root ^ "test-0004") ()
    |> IntMap.add "a" 1
    |> IntMap.add "b" 2
    |> IntMap.size
    |> Alcotest.(check int) "Size behaves reasonably" 2;

    IntMap.empty ~directory:(root ^ "test-0005") ()
    |> IntMap.add "one_hundred" 100
    |> IntMap.mem "one_hundred"
    |> Alcotest.(check bool) "Added values are members of the set" true;

    (IntMap.empty ~directory:(root ^ "test-0006") (), IntMap.empty ~directory:(root ^ "test-0007") ())
    |> (fun (s1, s2) -> (IntMap.add "a" 10 s1, s2))
    |> (fun (s1, s2) -> (IntMap.add "b" 5 s1, s2))
    |> (fun (s1, s2) -> (s1, IntMap.add "a" 1 s2))
    |> (fun (_, s2) -> IntMap.values s2)
    |> Alcotest.(check (list int)) "Sets don't interfere with each other" [1];

    IntMap.empty ~directory:(root ^ "test-0008") ()
    |> IntMap.add "a" 5
    |> IntMap.add "b" 32
    |> IntMap.add "c" (-15)
    |> IntMap.values
    |> List.sort compare
    |> Alcotest.(check (list int)) "Contents are retrieved correctly" [-15; 5; 32];
  end

let test_base () =
  Alcotest.(check string) "The random name function begins with `server--`"
	  (String.sub (Server.random_server_name()) 0 8) "server--"

let suite = [
	"type", [
   "set", `Quick, test_set;
	 "base", `Quick, test_base;
	]
]

let () = Alcotest.run "trace" suite
