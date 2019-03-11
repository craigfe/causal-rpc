open Lwt.Infix
open Trace_rpc
open Intmap

module I = IntPair (Trace_rpc_unix.Make)(Helpers.GitBackend)
open I

let create_client directory remote =
  IntClient.empty ~directory
    ~local_uri:("file://" ^ directory)
    ~remote_uri:("file://" ^ remote)
    ~name:"clientA"
    ~initial:Int64.one

let test_single_rpc _switch () =
  let root = "/tmp/irmin/test_unicast/single_rpc/" in

  (* Create a simple client *)
  create_client (root ^ "clientA") (root ^ "server")
  >>= fun client -> IntMap.empty ~directory:(root ^ "server") ()
  >>= fun server -> IntMap.start server
  >>= fun () -> IntClient.rpc multiply_op (Interface.Param (Type.int64, Int64.of_int 10, Interface.Unit)) client
  >|= Int64.to_int
  >|= Alcotest.(check int) "Something" 10

  >>= (fun () ->
  let rec inner n max =
    if n = max then Lwt.return_unit
    else
      IntClient.rpc increment_op Interface.Unit client
      >|= Int64.to_int
      >|= Alcotest.(check int) "Something" (n+1)
      >|= (fun () -> print_endline @@ Fmt.strf "%a" Core.Time_ns.pp (Core.Time_ns.now ()))
      >>= fun _ -> inner (n+1) max
  in inner 10 100)
  >>= fun () -> IntClient.output client

let tests = [
  Alcotest_lwt.test_case "Single client RPC" `Quick test_single_rpc
]

