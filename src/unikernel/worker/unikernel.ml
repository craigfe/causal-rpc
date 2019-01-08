open Lwt.Infix
open Mirage_types_lwt
open Trace_rpc

module Main (T : TIME) (P: PCLOCK) (_: Resolver_lwt.S) (_: Conduit_mirage.S) = struct

  let start _time pclock resolver conduit _ =
    let client = Key_gen.client () in

    let module Context = struct
      let r = resolver
      let c = conduit
      let p = pclock
    end in
    let module I = Intmap.IntPair (Trace_rpc_mirage.Make(T)(P)(Context)) (Irmin_git.Mem) in
    let open I in
    let open Intmap in

    Misc.set_reporter ();
    Logs.set_level (Some Logs.Info);

    IntMap.empty ~directory:("/tmp/irmin/test") ()
    >>= IntMap.add_all ["a", Int64.of_int 0;
                        "b", Int64.of_int 1;
                        "c", Int64.of_int 2;
                        "d", Int64.of_int 3;
                        "e", Int64.of_int 4]
    >>= fun m -> Lwt.pick [
      IntWorker.run ~client ();
      IntMap.map multiply_op (Interface.Param (Type.int64, Int64.of_int 10, Interface.Unit)) m
      >|= fun _ -> ()
    ]
    >>= fun () -> IntMap.values m
    >|= List.map Int64.to_int
    >|= List.sort compare
    >|= fun vs -> Logs.app (fun f -> f "Done with final values: %a" (Fmt.brackets @@ Fmt.list ~sep:Fmt.comma Fmt.int) vs)
end
