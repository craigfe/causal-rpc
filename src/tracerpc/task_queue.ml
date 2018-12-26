
type task = {
  name: string; [@compare fun _ _ -> 0]
  params: Type.Boxed.t list; [@compare fun _ _ -> 0]
  key: string;
} [@@deriving eq, ord]

let show_task t = t.key
let pp_task = Fmt.using show_task Fmt.string

let task =
  let open Irmin.Type in
  record "task" (fun name params key -> { name; params; key })
  |+ field "name" string (fun t -> t.name)
  |+ field "params" (list Type.Boxed.irmin_t) (fun t -> t.params)
  |+ field "key" string (fun t -> t.key)
  |> sealr

module TaskSet = Set.Make(struct type t = task let compare = compare_task end)
let pp_taskset = Fmt.using TaskSet.elements (Fmt.braces (Fmt.list ~sep:Fmt.comma pp_task))

let task_testable = Alcotest.testable pp_task equal_task

type t = (task list * task list) [@@deriving show]
let t = Irmin.Type.(pair (list task) (list task))
let t_testable = Alcotest.(pair (list task_testable) (list task_testable))

let of_internal = TaskSet.of_list
let to_internal x = List.sort compare_task (TaskSet.elements x)

(* type internal = TaskSet.t * TaskSet.t *)
let pp_internal = Fmt.braces @@ Fmt.pair ~sep:Fmt.comma pp_taskset pp_taskset

type operations = {
  consume: TaskSet.t; [@printer pp_taskset]
  perform: TaskSet.t; [@printer pp_taskset]
} [@@deriving show]

let compute_operations ~input ~output =
  let todo_in = of_internal (fst input) in
  let pending_in = of_internal (snd input) in
  let todo_out = of_internal (fst output) in
  let pending_out = of_internal (snd output) in

  let (-) = TaskSet.diff in
  let (+) = TaskSet.union in

  let consume = todo_in - todo_out in
  let perform = (todo_in + pending_in) - (todo_out + pending_out) in
  {consume; perform}

let apply_operations (todo, pending) {consume; perform} =
  let (>) = fun a b -> not (TaskSet.subset a b) in
  let (-) = TaskSet.diff in
  let (+) = TaskSet.union in

  if consume > todo then Error "Attempting to consume tasks that don't exist in the todo queue"
  else if perform > (pending + consume) then Error "Attempting to perform tasks that don't exist in the pending queue"
  else Ok (todo - consume, (pending + consume) - perform)

let merge_operations x y =
  let (+) = TaskSet.union in
  let {consume = consume_x; perform = perform_x} = x in
  let {consume = consume_y; perform = perform_y} = y in

  {consume = consume_x + consume_y; perform = perform_x + perform_y}

let merge ~old x y =
  let open Irmin.Merge.Infix in
  old () >>=* fun old ->
  let old = match old with None -> ([], []) | Some o -> o in

  Logs.debug (fun m -> m "Computing operations from \n%a\n to \n%a\n" pp old pp x);

  let ops_x = compute_operations ~input:old ~output:x in
  let ops_y = compute_operations ~input:old ~output:y in
  let old = (TaskSet.of_list (fst old), TaskSet.of_list (snd old)) in

  Logs.debug (fun m -> m "Merging: \n%a] and \n%a" pp_operations ops_x pp_operations ops_y);

  merge_operations ops_x ops_y
  |> (fun o -> begin
        Logs.debug (fun m -> m "Got merged ops: %a" pp_operations o);
        apply_operations old o
      end)
  |> (fun x -> begin
        Logs.debug (fun m -> m "Applied ops to %a to get %a" pp_internal old (Fmt.result ~ok:pp_internal ~error:Fmt.string) x);
        match x with
        | Ok x -> Irmin.Merge.ok (to_internal (fst x), to_internal (snd x))
        | Error c -> Irmin.Merge.conflict "%s" c
      end)


