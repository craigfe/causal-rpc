open Lwt.Infix
open Trace_rpc.Task
open Trace_rpc.Task_queue

type conflict = [
  | `Conflict of string
] [@@deriving show]

let conflict_equal (`Conflict a) (`Conflict b) = String.equal a b
let merge_conflict = Alcotest.testable pp_conflict conflict_equal
let merge_t = Alcotest.(result t_testable merge_conflict)

let mk_task k =
  {name = "op_name"; params = []; key = k }

let mk_queue (x, y) =
  ( List.sort String.compare x |> List.map mk_task
  , List.sort String.compare y |> List.map mk_task )

let merge_check ?old ~a ~b ?res description =
  let a = mk_queue a in
  let b = mk_queue b in
  let old = match old with (* Default old to a *)
    | Some o -> Irmin.Merge.promise (mk_queue o)
    | None -> Irmin.Merge.promise a in

  let res = match res with (* Default res to b *)
    | Some r -> Ok (mk_queue r)
    | None -> Ok b in

  merge ~old a b
  >|= Alcotest.check merge_t description res

let test_merge _ () =
  let a = (["a"], []) in
  let b = (["a"], []) in
  merge_check ~a ~b "Identity operation"

  >>= fun () ->
  let a = (["a"; "b"; "c"], ["d"; "e"; "f"]) in
  let b = (["a"; "b"; "c"], ["d"; "e"; "f"]) in
  merge_check ~a ~b "Identity operation 2"

  >>= fun () ->
  let a = (["a"], []) in
  let b = ([], ["a"]) in
  merge_check ~a ~b "Single task consumed"

  >>= fun () ->
  let a = (["a"; "b"], ["c"]) in
  let b = (["b"], ["a"; "c"]) in
  merge_check ~a ~b "Single task consumed with prior tasks"

  >>= fun () ->
  let a = (["a"; "b"; "c"], []) in
  let b = (["c"], ["b"; "a"]) in
  merge_check ~a ~b "Multiple tasks consumed"

  >>= fun () ->
  let a = (["b"], ["a"]) in
  let b = (["b"], []) in
  merge_check ~a ~b "Single task performed"

  >>= fun () ->
  let a = (["c"], ["b"; "a"]) in
  let b = (["c"], []) in
  merge_check ~a ~b "Multiple tasks performed"

  >>= fun () ->
  let a = (["d"; "e"; "f"], ["c"; "b"; "a"]) in
  let b = (["f"], ["e"]) in
  merge_check ~a ~b "Multiple tasks consumed and performed"

  >>= fun () ->
  let old = (["a"; "b"], []) in
  let a = (["b"], ["a"]) in
  let b = (["b"], ["a"]) in
  let res = (["b"], ["a"]) in
  merge_check ~old ~a ~b ~res "Single task consumed on both branches"

  >>= fun () ->
  let old = (["a"], ["b"; "c"]) in
  let a = (["a"], ["c"]) in
  let b = (["a"], ["c"]) in
  let res = (["a"], ["c"]) in
  merge_check ~old ~a ~b ~res "Single task performed on both branches"

  >>= fun () ->
  let old = (["a"; "b"], []) in
  let a = (["b"], []) in
  let b = (["b"], ["a"]) in
  let res = (["b"], []) in
  merge_check ~old ~a ~b ~res "Task performed on one branch and consumed on another"

  >>= fun () ->
  let old = (["a"; "b"], []) in
  let a = (["b"], ["a"]) in
  let b = (["b"], []) in
  let res = (["b"], []) in
  merge_check ~old ~a ~b ~res "Task consumed on one branch and completed on another"

  >>= fun () ->
  let old = (["a"; "b"], ["c"; "d"]) in
  let a = ([], ["b"; "a"; "c"; "d"]) in
  let b = (["a"; "b"], []) in
  let res = ([], ["b"; "a"]) in
  merge_check ~old ~a ~b ~res "Multiple tasks consumed on one branch and performed on another"

  >>= fun () ->
  let old = (["a"; "b"], ["c"; "d"]) in
  let a = ([], ["b"; "a"; "c"; "d"]) in
  let b = (["b"], []) in
  let res = ([], ["b"]) in
  merge_check ~old ~a ~b ~res "Multiple tasks consumed/performed on one branch and performed on another"

  >>= fun () ->
  let old = (["a"; "b"; "c"], ["d"; "e"; "f"]) in
  let a = (["c"], ["b"; "e"; "f"]) in (* Perform d, Consume a, Perform a, Consume b *)
  let b = (["b"; "c"], ["f"]) in (* Consume a, Perform a, Perform d, Perform e *)
  let res = (["c"], ["b"; "f"]) in
  merge_check ~old ~a ~b ~res "Multiple tasks consumed/performed on both branches"

(* let test_map _ () =
 *   let root = "/tmp/irmin/task_queues/" in
 * 
 *   IntMap.empty ~directory:(root ^ "test-0001") ()
 *   >>= IntMap.add "a" Int64.one
 *   >>= IntMap.generate_task_queue Intmap.increment_op Operation.Unit (\* TODO: It shouldn't be necessary to pass the empty list here *\)
 * 
 *   >|= (fun c -> match c with
 *       | Task_queue (s, []) ->
 *         (\* Mangle the record into nested pairs so that alcotest can check equality *\)
 *         List.map (fun ({name;params;key}:Task_queue.task) -> (name, (params, key))) s
 *         |> Alcotest.(check (list (pair
 *                                     Alcotest.pass (\* TODO: implement Operation testable *\)
 *                                     (pair (list Type.Boxed.test_t) string)
 *                                  )))
 *           "Task queues are generated in the expected format"
 *           ["", ([],"a")]
 *       | Task_queue (_, _) -> Alcotest.fail "Generated task queue had finished items"
 *       | _ -> Alcotest.fail "Generate_task_queue returned a non-task value") *)



let tests = [
  Alcotest_lwt.test_case "Merge functions" `Quick test_merge;
  (* Alcotest_lwt.test_case "Map task queue format" `Quick test_map; *)
]
