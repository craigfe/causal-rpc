open Trace_rpc

(** Tests on the IntSet datastructure *)
let test_set () =
  let open Intset in begin
    let root = "/tmp/irmin/set/" in

    IntSet.empty ~directory:(root ^ "test-0001") ()
    |> IntSet.is_empty
    |> Alcotest.(check bool) "The empty set is empty" true;

    IntSet.empty ~directory:(root ^ "test-0002") ()
    |> IntSet.add(1)
    |> IntSet.is_empty
    |> Alcotest.(check bool) "A non-empty set is not empty" false;

    IntSet.empty ~directory:(root ^ "test-0003") ()
    |> IntSet.size
    |> Alcotest.(check int) "The empty set has size 0" 0;

    IntSet.empty ~directory:(root ^ "test-0004") ()
    |> IntSet.add(1)
    |> IntSet.add(2)
    |> IntSet.size
    |> Alcotest.(check int) "Size behaves reasonably" 2;

    IntSet.empty ~directory:(root ^ "test-0005") ()
    |> IntSet.add(100)
    |> IntSet.mem 100
    |> Alcotest.(check bool) "Added values are members of the set" true;

    (IntSet.empty ~directory:(root ^ "test-0006") (), IntSet.empty ~directory:(root ^ "test-0007") ())
    |> (fun (s1, s2) -> (IntSet.add 10 s1, s2))
    |> (fun (s1, s2) -> (IntSet.add 5 s1, s2))
    |> (fun (s1, s2) -> (s1, IntSet.add 1 s2))
    |> (fun (_, s2) -> IntSet.elements s2)
    |> Alcotest.(check (list int)) "Sets don't interfere with each other" [1];

    IntSet.empty ~directory:(root ^ "test-0008") ()
    |> IntSet.add(5)
    |> IntSet.add(32)
    |> IntSet.add(-15)
    |> IntSet.elements
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
