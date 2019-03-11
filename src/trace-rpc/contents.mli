(* Helper types and values for a CauslRPC store *)
type job_queue = Job.t list

type 'v t =
  | Value of 'v
  | Task_queue of Task_queue.t
  | Job_queue of job_queue

module Make(Val: Irmin.Contents.S): Irmin.Contents.S
  with type t = Val.t t

(* The job queue holds the active jobs to be performed *)
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
