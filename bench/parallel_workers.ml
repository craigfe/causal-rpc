
open Trace_rpc
open Intmap
open Lwt.Infix

let worker ~switch ~batch_size ~thread_count = IntWorker.run
    ~switch
    ~batch_size
    ~thread_count
    ~dir:"/tmp/irmin/bench/threading/worker"
    ~client:"file:///tmp/irmin/bench/threading/client"
    ~poll_freq:0.01 ()


let maprequest threads () =
  Random.self_init ();
  Logs.set_reporter Logs.nop_reporter;
  Logs.set_level (Some Logs.App);

  let lwt =
    let switch = Lwt_switch.create () in
    let values = Helpers.sequence_list 1 8 in
    let keys = List.map Helpers.key_from_int values in

    IntMap.empty ~directory:("/tmp/irmin/bench/threading/client") ()
    >>= IntMap.map sleep_op (Interface.Param (Type.float, 10., Interface.Unit))
    >>= IntMap.add_all (Misc.zip keys (List.map Int64.of_int values))
    >>= fun m -> Lwt.pick [
      worker ~switch ~batch_size:threads ~thread_count:threads;
      IntMap.map ~timeout:50.0 multiply_op (Interface.Param (Type.int64, Int64.of_int 10, Interface.Unit)) m
      >>= fun _ -> Lwt_switch.turn_off switch
    ]
    >|= fun () -> ()
  in Lwt_main.run lwt


let () =
  maprequest 4 ()
