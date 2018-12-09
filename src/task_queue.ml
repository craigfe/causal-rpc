
type task = {
  name: string;
  params: Type.Boxed.t list;
  key: string;
} [@@deriving show, eq]

let task =
  let open Irmin.Type in
  record "task" (fun name params key -> { name; params; key })
  |+ field "name" string (fun t -> t.name)
  |+ field "params" (list Type.Boxed.irmin_t) (fun t -> t.params)
  |+ field "key" string (fun t -> t.key)
  |> sealr

let task_testable = Alcotest.testable pp_task equal_task

type t = (task list * task list) [@@deriving show, eq]
let t = Irmin.Type.(pair (list task) (list task))
let t_testable = Alcotest.(pair (list task_testable) (list task_testable))

type operation = Consume of task | Perform of task [@@deriving show, eq]

let compute_operations ~input ~output =
  let rec inner x y acc = match x, y with

    | x, y when equal x y -> Ok (List.rev acc)

    | (t::ts, []), (us, qs) -> inner (ts, [t]) (us, qs) (Consume t::acc)
    | (ts, p::ps), (us, []) -> inner (ts, ps) (us, []) (Perform p::acc)

    | (ts, p::ps), (us, qs) when not (List.exists (equal_task p) qs)
      -> inner (ts, ps) (us, qs) (Perform p::acc)

    | (t::ts, ps), (us, qs) when not (List.exists (equal_task t) us)
      -> inner (ts, t::ps) (us, qs) (Consume t::acc)

    (* | (t::ts, ps), ([], qs) -> inner (ts, t::ps) ([], qs) (Consume t::acc) *)

    | (t::ts, ps), (u::us, qs) when not (equal_task t u)
      -> inner (ts, t::ps) (u::us, qs) (Consume t::acc)

    | x, y -> Error (Format.asprintf "\nWhen attempting to compute operations from %a\n to %a.\n Could not compute operations from %a\n to %a" pp input pp output pp x pp y)

  in inner input output []

let rec apply_ops queue ops = match queue, ops with
  | q, [] -> q
  | (t::ts, ps), (Consume x)::ops when equal_task t x -> apply_ops (ts, t::ps) ops
  | (ts, p::ps), (Perform x)::ops when equal_task p x -> apply_ops (ts, ps) ops
  | _ -> invalid_arg (Format.asprintf "Failed to apply %a to %a" (Fmt.list pp_operation) ops pp queue)

let rec merge_ops x y = match x, y with
  | xs, [] -> xs
  | [], ys -> ys
  | x::xs, y::ys when equal_operation x y -> x::(merge_ops xs ys)
  | (Consume x)::xs, (Perform y)::ys when equal_task x y -> (Perform y)::(merge_ops xs ys)
  | (Perform x)::xs, (Consume y)::ys when equal_task x y -> (Perform x)::(merge_ops xs ys)

  | (Perform x)::xs, ys -> (Perform x)::(merge_ops xs ys)
  | xs, (Perform y)::ys -> (Perform y)::(merge_ops xs ys)

  | _ -> invalid_arg "Failed to merge ops"

let merge ~old x y =
  let open Irmin.Merge.Infix in
  old () >>=* fun old ->
  let old = match old with None -> ([], []) | Some o -> o in

  let ops_x = compute_operations ~input:old ~output:x in
  let ops_y = compute_operations ~input:old ~output:y in

  let pp_r = (Fmt.result ~ok:(Fmt.list pp_operation) ~error:Fmt.string) in
  Logs.info (fun m -> m "Merging: [%a] and [%a]"  pp_r ops_x pp_r ops_y);

  match ops_x, ops_y with
  | Ok [], Ok [] -> Irmin.Merge.ok old
  | Ok _ , Ok [] -> Irmin.Merge.ok x
  | Ok [], Ok _  -> Irmin.Merge.ok y
  | Ok x, Ok y ->
    merge_ops x y
    |> fun o -> Logs.info (fun m -> m "got merged ops: %a" (Fmt.list pp_operation) o); apply_ops old o
    |> fun x -> Logs.info (fun m -> m "applied ops to %a to get %a" pp old pp x); Irmin.Merge.ok x

  | Error x, _ -> invalid_arg ("Could not compute operations for x: " ^ x)
  | _, Error y -> invalid_arg ("Could not compute operations for y: " ^ y)

