open Lwt.Infix
open Intmap

let key_from_int n =
  let rec inner n = match n with
    | 0 -> []
    | n -> n mod 26
           |> (+) (int_of_char 'a' - 1)
           |> char_of_int
           |> String.make 1
           |> fun s -> s::(inner (n / 26))

  in String.concat "" (inner n)

let sequence_list lower upper =
  let rec helper n acc = match n with
    | n when n = lower -> lower::acc
    | n                -> helper (n-1) (n::acc)
  in helper upper []

let stress_test () =
  let root = "/tmp/irmin/test_stress/stress/" in
  let values = sequence_list 1 12 in
  let keys = List.map key_from_int values in

  IntMap.empty ~directory:root ()
  >>= IntMap.add_all (Trace_rpc.Misc.zip keys (List.map Int64.of_int values))
  >>= fun m -> let rec inner n map =
        IntMap.map ~timeout:100.0 Trace_rpc.Intmap.increment_op Trace_rpc.Interface.Unit map
        >>= IntMap.values
        >|= List.map Int64.to_int
        >|= List.sort compare
        >>= fun actual -> Lwt.return (List.map ((+) n) values)
        >>= fun expected -> Lwt.return (Alcotest.(check (list int)) "Multiple request on many keys concurrently" expected actual)
        >>= fun () -> Logs_lwt.app (fun m -> m "Iteration %i complete" n)
        >>= fun () -> inner (n + 1) map
      in inner 1 m

let () =
  Lwt_main.run (stress_test ())
