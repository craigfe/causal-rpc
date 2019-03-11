type job_queue = Job.t list
let job_queue = Irmin.Type.list Job.t

type 'v t =
  | Value of 'v
  | Task_queue of Task_queue.t
  | Job_queue of job_queue

module type JOB_QUEUE = sig
  module Store: Irmin.KV

  module type IMPL = sig
    val is_empty: Store.t -> bool Lwt.t
    val push: Job.t -> Store.t -> unit Lwt.t
    val pop: Store.t -> (Job.t, string) result Lwt.t
    val pop_silent: Store.t -> (Job.t * Job.t list) Lwt.t
    val peek_opt: Store.t -> Job.t option Lwt.t
  end

  module Impl: IMPL
end

type 'a cntnts = 'a t (* Disambiguate <t>s *)
module Make (Val: Irmin.Contents.S): Irmin.Contents.S
  with type t = Val.t cntnts = struct

  type t = Val.t cntnts

  let t =
    let open Irmin.Type in
    variant "contents" (fun value task_queue job_queue -> function
        | Value v -> value v
        | Task_queue q -> task_queue q
        | Job_queue js -> job_queue js)
    |~ case1 "Value" Val.t (fun v -> Value v)
    |~ case1 "Task_queue" Task_queue.t (fun q -> Task_queue q)
    |~ case1 "Job_queue" job_queue (fun js -> Job_queue js)
    |> sealv

  let merge ~old t1 t2 =

    let open Irmin.Merge.Infix in
    old () >>=* fun old ->

    match (old, t1, t2) with
    | Some Value o, Value a, Value b ->

      (* TODO: work out why optional merge combinators are default in Irmin.Contents.S *)
      (Irmin.Merge.f Val.merge) ~old:(Irmin.Merge.promise (Some o)) (Some a) (Some b)
      >>=* fun x -> (match x with
          | Some x -> Irmin.Merge.ok (Value x)
          | None -> invalid_arg "no value")

    | Some Task_queue o, Task_queue a, Task_queue b ->

      Task_queue.merge ~old:(Irmin.Merge.promise o) a b
      >>=* fun x -> Irmin.Merge.ok (Task_queue x)

    (* Irmin.Merge.conflict "%s" (Format.asprintf "old = %a\n\na = %a\n\nb = %a" pp_task_queue o pp_task_queue a pp_task_queue b) *)

    | _, Job_queue _, Job_queue _ -> Irmin.Merge.conflict "Job_queue"
    | _ -> Irmin.Merge.conflict "Different types in store"

  let merge = Irmin.Merge.(option (v t merge))
end
