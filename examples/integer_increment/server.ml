open Trace_rpc

let main () =
  let open Intmap in
  IntMap.empty ~directory:"/tmp/irmin/integer_increment" ()
  |> IntMap.add "a" (Int64.of_int 1)
  |> IntMap.add "b" (Int64.of_int 10)
  |> IntMap.add "c" (Int64.of_int 100)
  |> IntMap.map "double" (* Perform an integer increment *)
  |> fun _ -> ()

let () =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Info);
  main ()
