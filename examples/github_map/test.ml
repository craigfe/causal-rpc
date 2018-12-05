open Api

open Lwt.Infix
let global_root = "/tmp/irmin/github_map/test/"
let worker dir = GithubWorker.run
    ~dir:(global_root ^ "/worker/" ^ dir)
    ~client:("file://" ^ global_root ^ dir)
    ~poll_freq:0.01 ()

let count_commit_test _ () =
  Trace_rpc.Misc.set_reporter ();
  Logs.set_level (Some Logs.Info);

  let root = global_root ^ "commit_count/" in

  GithubMap.empty ~directory:(root ^ "test-0001") ()
  |> fun m -> Github.get_repos

  (* Temporary hack until we have input/output variant types *)
  >|= List.map (fun (name, url) -> (name, (url, 0)))
  >|= (fun repos -> GithubMap.add_all repos m)
  >>= fun m -> Lwt.pick [
    worker "commit_count/test-0001";
    GithubMap.map commit_count_op Trace_rpc.Interface.Unit m
    >|= fun _ -> ()
  ]
  >|= fun _ -> ()


let suite = [
 (*  "count_commits", [
  *    Alcotest_lwt.test_case "basic" `Quick count_commit_test
  * ] *)
]

let () = Alcotest.run "trace" suite
