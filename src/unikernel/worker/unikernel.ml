(* open Lwt.Infix *)
open Mirage_types_lwt

module Main (T : TIME) (_: Resolver_lwt.S) (_: Conduit_mirage.S) = struct

  let start _time resolver conduit _ =
    let client = Key_gen.client () in

    let module Context = struct let r = resolver let c = conduit end in
    let module I = Trace_rpc.Intmap.IntPair (Trace_rpc_mirage.Make(T)(Context)) (Irmin_git.Mem) in
    let open I in

    Trace_rpc.Misc.set_reporter ();
    Logs.set_level (Some Logs.Info);
    IntWorker.run ~client ()
end
