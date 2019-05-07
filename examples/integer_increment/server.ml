open Helpers

let main =
  IntMap.empty ~directory:"/tmp/irmin/example/server" ()
  >>= add "a" 1
  >>= add "b" 10
  >>= add "c" 100

  (* Perform an integer increment on the map *)
  >>= IntMap.map ~timeout:5.0 ~polling:true (mult 5)
  >|= fun _ -> ()

let () =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Info);
  Lwt_main.run main
