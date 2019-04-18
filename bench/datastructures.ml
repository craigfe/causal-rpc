open Lwt.Infix
open Core_bench

type task = {
  name: string;
  params: string list;
  key: string;
}

let task =
  let open Irmin.Type in
  record "task" (fun name params key -> { name; params; key })
  |+ field "name" string (fun t -> t.name)
  |+ field "params" (list string) (fun t -> t.params)
  |+ field "key" string (fun t -> t.key)
  |> sealr

module List: Irmin.Contents.S with type t = task list = struct
  type t = task list
  let t = Irmin.Type.(list task)
  let merge = Irmin.Merge.idempotent Irmin.Type.(option t)
end

(* Our fast task queue uses blobs of type 'task' *)
module Task: Irmin.Contents.S with type t = task = struct
  type t = task
  let t = task
  let merge = Irmin.Merge.idempotent Irmin.Type.(option t)
end

module S = Irmin_unix.Git.FS.KV(List)
module S_fast = Irmin_unix.Git.FS.KV(Task)


let reverse l =
  let rec inner = function
    | ([], acc) -> acc
    | (h::ts, acc) -> inner (ts, h::acc)
  in inner (l, [])

let listGen n =
  let rec inner i acc =
    if i = n then acc
    else inner (i+1) (
        {name="increment";params=["this"; "is"; "a"; "large"; "list"];key=string_of_int(i)}::acc)
  in reverse (inner 0 [])

let slow_task len: unit -> unit =
  let ret_code = Sys.command ("rm -rf /tmp/irmin/bench_data/slow_task" ^ string_of_int len) in
  if (ret_code <> 0) then invalid_arg "Unable to delete directory";

  let config = Irmin_git.config ~bare:true ("/tmp/irmin/bench_data/slow_task" ^ string_of_int len) in
  let lst = listGen len in
  (* Construct a list with this length, and store it in an Irmin store *)
  let t = Lwt_main.run (
      S.Repo.v config
      >>= fun repo -> S.master repo
      >>= fun t -> S.set ~info:(Irmin_unix.info "Initial value") t ["root"] lst
      >>= fun _result -> S.get_tree t ["root"]
      >|= S.Tree.clear_caches
      >|= fun () -> t
    ) in

  (* Deserialise the list *)
  fun () -> Lwt_main.run (
      S.set ~info:(Irmin_unix.info "Reset") t ["root"] lst
      >>= fun _result -> S.get_tree t ["root"]
      >|= (fun tree -> match tree with
      | `Contents (_::ts, _) -> S.set ~info:(Irmin_unix.info "Popped") t ["root"] ts
      | `Contents([], _) -> invalid_arg "Empty list"
      | `Node _ -> invalid_arg "Incorrect type")

      >>= fun _result -> S.get_tree t []
      >|= S.Tree.clear_caches
    )

(* let fast_task len: unit -> unit =
 *   let branch_factor = 10 in
 * 
 *   let ret_code = Sys.command ("rm -rf /tmp/irmin/bench_data/fast_task" ^ string_of_int len) in
 *   if (ret_code <> 0) then invalid_arg "Unable to delete directory"; *)

let tests = [
  "Slow task",
  (fun len -> Core.Staged.stage (slow_task len));
  (* "Fast task"
   * (fun len -> Core.Staged.stage (fast_task task)) *)
]

let batch_sizes =
  let res = ref [] in
  let count = ref 10_000 in
  while !count >= 5 do
    res := (!count :: !res);
    count := int_of_float (float_of_int (!count) /. 1.5)
  done;
  !res

let () =
  Core.List.map tests ~f:(fun (name,test) -> Bench.Test.create_indexed ~name ~args:batch_sizes test)
  |> Bench.make_command
  |> Core.Command.run
