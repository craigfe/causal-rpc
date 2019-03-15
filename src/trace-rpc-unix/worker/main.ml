module I = Trace_rpc.Intmap.IntPair (Trace_rpc_unix.Make) (Irmin_unix.Git.FS.G)
open I

let () =
  Trace_rpc.Helpers.set_reporter ();
  Logs.set_level (Some Logs.Info);
  Lwt_main.run (IntWorker.run ~client:Sys.argv.(1) ())
