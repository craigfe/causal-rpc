open Trace_rpc
open Misc

(** Simple tests of miscellaneous features of the library *)
let test_names () =
  Alcotest.(check string) "The random name function begins with `server--`"
	  (String.sub (Server.random_server_name()) 0 8) "server--"

let test_split_sequential () =
  let test = Alcotest.(check (pair (list int) (list int))) in

  split_sequential 0 []
  |> test "Empty list" ([], []);

  split_sequential 0 [1;2;3;4;5]
  |> test "Empty partition" ([], [1;2;3;4;5]);

  split_sequential 5 [1;2;3;4;5]
  |> test "Full partition" ([1;2;3;4;5], []);

  split_sequential 1 [1;2;3]
  |> test "Non-trivial partition 1" ([1], [2;3]);

  split_sequential 2 [1;2;3]
  |> test "Non-trivial partition 2" ([1;2], [3])

let test_split_random () =
  let test_split_sizes desc expected (x, y) =
    (List.length x, List.length y)
    |> Alcotest.(check (pair int int)) desc expected
  in

  split_sequential 0 []
  |> test_split_sizes "Empty list" (0, 0);

  split_sequential 0 [1;4;3;2;5]
  |> test_split_sizes "Empty partition" (0, 5);

  split_sequential 5 [5;3;4;2;1]
  |> test_split_sizes "Full partition" (5, 0);

  split_sequential 1 [1;3;1]
  |> test_split_sizes "Non-trivial partition 1" (1, 2);

  split_sequential 2 [1;3;1]
  |> test_split_sizes "Non-trivial partition 2" (2, 1)

let tests = [
  "Name generation", `Quick, test_names;
  "Sequential list partitioning", `Quick, test_split_sequential;
  "Random list partitioning", `Quick, test_split_random;
]
