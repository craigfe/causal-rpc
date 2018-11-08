
open Trace_rpc

let main () =
  let open Intmap in
  IntMap.empty ~directory:"/tmp/irmin/integer_increment" ()
  |> IntMap.add "a" 1
  |> IntMap.add "b" 10
  |> IntMap.add "c" 100
  |> IntMap.map (* Perform an integer increment *)
  |> fun _ -> ()

let () = main ()
